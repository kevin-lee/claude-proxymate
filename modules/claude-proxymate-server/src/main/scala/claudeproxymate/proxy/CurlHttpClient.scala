package claudeproxymate.proxy

import cats.effect.{FiberIO, IO, Ref, Resource}
import cats.syntax.all.*
import claudeproxymate.core.{ProxyError, UpstreamHead, UpstreamHeadError, UpstreamHeadParse}
import fs2.{Chunk, Stream}
import org.http4s.{Header, Headers, Method, Request, Response, Status}
import org.http4s.client.Client
import org.typelevel.ci.*

import scala.concurrent.duration.*
import scala.scalanative.libc.{errno as libcErrno, stdio, stdlib, string}
import scala.scalanative.posix.{errno as posixErrno, unistd}
import scala.scalanative.posix.stdio as posixStdio
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.UnsignedRichInt

/** An http4s `Client[IO]` on libcurl's easy API that streams the response
  * body (issue #100).
  *
  * For each request:
  *
  *   1. libcurl writes the response, status line and headers first
  *      (`CURLOPT_HEADER`), with its default write function: the C `fwrite`
  *      into the write end of a pipe (`CURLOPT_WRITEDATA`). The write end is
  *      unbuffered, so each chunk goes into the pipe at once.
  *   1. `curl_easy_perform` (with `@blocking`) runs in `IO.blocking` on its own
  *      fiber. When it returns, the write end is closed, and the reader gets
  *      end of file.
  *   1. The reader reads the read end with `read(2)` (with `@blocking`) in
  *      `IO.blocking`, parses the header blocks with [[UpstreamHead.parse]],
  *      and gives the rest as the body stream.
  *
  * No Scala code runs inside `curl_easy_perform`: the write path is C, and the
  * only callback is the C transfer-info function in
  * `claudeproxymate_curl_transfer.c`, which reads an abort flag.
  *
  * Each stream uses two blocking threads for its full time. Requests share no
  * state, so parallel requests from many Claude Code instances are independent.
  *
  * Release (when the body stream ends, fails or is cancelled, or when the
  * body is never read, see [[BodyGuard]]): set the abort flag, so libcurl
  * stops within about one second also when no data comes, wait for a read
  * that is in progress, close the read end, so an `fwrite` that waits on a full
  * pipe fails, then wait for `curl_easy_perform`, and only then free the native
  * memory.
  */
object CurlHttpClient {

  /** Limits of one transfer.
    *
    * @param connectTimeout  `CURLOPT_CONNECTTIMEOUT`
    * @param idleTimeout     `CURLOPT_LOW_SPEED_TIME` with `CURLOPT_LOW_SPEED_LIMIT`
    *                        1 byte/s: the transfer stops when it gets less than
    *                        1 byte/s for this time. There is no limit on the
    *                        total time.
    * @param readBufferBytes the size of one `read(2)` from the pipe
    * @param maxHeadBytes    the maximum length of the header blocks
    */
  final case class Settings(
    connectTimeout: FiniteDuration,
    idleTimeout: FiniteDuration,
    readBufferBytes: Int,
    maxHeadBytes: Int,
  )

  object Settings {

    /** `idleTimeout` is the default of Claude Code's `API_TIMEOUT_MS` (600 s).
      * A non-streaming request gets no bytes until the response is complete,
      * so the limit must not be shorter than Claude Code's own wait.
      */
    val Default: Settings =
      Settings(
        connectTimeout = 30.seconds,
        idleTimeout = 600.seconds,
        readBufferBytes = 64 * 1024,
        maxHeadBytes = 64 * 1024
      )
  }

  /** Initialise libcurl and ignore `SIGPIPE`, once, before the first request.
    *
    * `curl_easy_init` calls `curl_global_init` itself when it was not called,
    * but that is thread-safe only in a libcurl with the `threadsafe` feature
    * (7.84.0 and later). `SIGPIPE` must be ignored, because the release closes
    * the read end of a pipe that libcurl can still write to.
    */
  val globalInit: IO[Unit] =
    IO(LibCurl.curl_global_init(CurlGlobal.All)).flatMap { code =>
      IO.raiseWhen(code =!= CurlCode.Ok)(new IllegalStateException(s"curl_global_init failed with code $code"))
    } *>
      IO(CurlTransferNative.claudeproxymate_ignore_sigpipe()).flatMap { result =>
        IO.raiseWhen(result =!= 0)(new IllegalStateException("signal(SIGPIPE, SIG_IGN) failed"))
      }

  def client(settings: Settings): Client[IO] =
    Client[IO] { (req: Request[IO]) =>
      Resource.eval(req.body.compile.to(Array)).flatMap(body => transfer(settings, req, body))
    }

  /* Request headers that are not forwarded. libcurl derives Host from the URL
   * and the length from CURLOPT_POSTFIELDSIZE (the request filter may have
   * changed the body). The empty "Accept-Encoding:" and "Expect:" added in
   * headerList remove those headers, so the body is not compressed and libcurl
   * does not wait for "100 Continue". */
  private val DroppedRequestHeaders: Set[CIString] =
    Set(ci"Host", ci"Accept-Encoding", ci"Content-Length", ci"Transfer-Encoding", ci"Expect")

  /* Response headers that are not forwarded. They describe the upstream
   * connection, not the body: libcurl already removed the chunked encoding, and
   * the http4s server sets its own framing for the client connection. */
  private val HopByHopResponseHeaders: Set[CIString] =
    Set(
      ci"Connection",
      ci"Keep-Alive",
      ci"Proxy-Connection",
      ci"Transfer-Encoding",
      ci"TE",
      ci"Trailer",
      ci"Upgrade",
    )

  final private case class Pipe(readFd: CInt, writer: Ptr[stdio.FILE])

  final private case class Running(readFd: CInt, perform: FiberIO[CInt])

  private enum ReadResult {
    case Bytes(chunk: Chunk[Byte])
    case EndOfFile
    case Failed(errno: CInt)
  }

  private enum Head {
    case Complete(head: UpstreamHead, rest: Chunk[Byte])
    case Invalid(error: UpstreamHeadError)
    case EndedEarly
  }

  private def transfer(settings: Settings, request: Request[IO], body: Array[Byte]): Resource[IO, Response[IO]] =
    for {
      curl     <- easyHandle
      flag     <- abortFlag
      list     <- headerList(request.headers)
      buffer   <- readBuffer(settings.readBufferBytes)
      inFlight <- Resource.eval(Ref.of[IO, Option[FiberIO[ReadResult]]](none[FiberIO[ReadResult]]))
      run      <- Resource.make(start(curl, request, body, list, flag, settings))(stop(flag, inFlight))
      response <- Resource.eval(respond(run, buffer, flag, inFlight, settings))
    } yield response

  private def easyHandle: Resource[IO, LibCurl.CURL] =
    Resource
      .make(IO(LibCurl.curl_easy_init()))(curl => IO(LibCurl.curl_easy_cleanup(curl)).whenA(curl != null))
      .evalTap(curl => IO.raiseWhen(curl == null)(ProxyErrorException(ProxyError.CurlInitFailed)))

  private def abortFlag: Resource[IO, Ptr[CInt]] =
    Resource
      .make(IO(stdlib.calloc(1.toUSize, sizeof[CInt]).asInstanceOf[Ptr[CInt]]))(flag => IO(stdlib.free(flag)))
      .evalTap(flag => IO.raiseWhen(flag == null)(new OutOfMemoryError("calloc for the curl abort flag failed")))

  private def readBuffer(bytes: Int): Resource[IO, Ptr[Byte]] =
    Resource
      .make(IO(stdlib.malloc(bytes.toUSize)))(buffer => IO(stdlib.free(buffer)))
      .evalTap(buffer => IO.raiseWhen(buffer == null)(new OutOfMemoryError("malloc for the pipe read buffer failed")))

  private def headerList(headers: Headers): Resource[IO, LibCurl.SList] =
    Resource.make(IO(buildHeaderList(headers)))(list => IO(LibCurl.curl_slist_free_all(list)).whenA(list != null))

  /* curl_slist_append copies each string, so the Zone can end here. */
  private def buildHeaderList(headers: Headers): LibCurl.SList =
    Zone {
      val forwarded = headers.headers.filterNot(h => DroppedRequestHeaders.contains(h.name))
      val lines     = forwarded.map(h => s"${h.name}: ${h.value}") ++ List("Accept-Encoding:", "Expect:")
      lines.foldLeft(null.asInstanceOf[LibCurl.SList]) { (list, line) =>
        LibCurl.curl_slist_append(list, toCString(line))
      }
    }

  private def openPipe: IO[Pipe] =
    IO {
      val fds = stackalloc[CInt](2)
      if (unistd.pipe(fds) =!= 0) {
        none[Pipe]
      } else {
        val writer = posixStdio.fdopen(fds(1), c"w")
        if (writer == null) {
          unistd.close(fds(0)): Unit
          unistd.close(fds(1)): Unit
          none[Pipe]
        } else {
          /* Unbuffered: each fwrite of libcurl goes into the pipe at once, so
           * an SSE event is not held in a stdio buffer. */
          stdio.setvbuf(writer, null, stdio._IONBF, 0.toUSize): Unit
          Pipe(fds(0), writer).some
        }
      }
    }.flatMap(_.liftTo[IO](ProxyErrorException(ProxyError.PipeFailed)))

  private def start(
    curl: LibCurl.CURL,
    request: Request[IO],
    body: Array[Byte],
    list: LibCurl.SList,
    flag: Ptr[CInt],
    settings: Settings,
  ): IO[Running] =
    openPipe.flatMap { pipe =>
      configure(curl, request, body, list, flag, pipe.writer, settings)
        .onError(_ => IO(stdio.fclose(pipe.writer)) *> IO(unistd.close(pipe.readFd)).void) *>
        IO.blocking(LibCurl.curl_easy_perform(curl))
          .guarantee(IO.blocking(stdio.fclose(pipe.writer)).void)
          .start
          .map(perform => Running(pipe.readFd, perform))
    }

  private def stop(flag: Ptr[CInt], inFlight: Ref[IO, Option[FiberIO[ReadResult]]])(run: Running): IO[Unit] =
    IO(CurlTransferNative.claudeproxymate_curl_abort(flag)) *>
      inFlight.get.flatMap(_.traverse_(_.join.void)) *>
      IO(unistd.close(run.readFd)).void *>
      run.perform.join.void

  private def configure(
    curl: LibCurl.CURL,
    request: Request[IO],
    body: Array[Byte],
    list: LibCurl.SList,
    flag: Ptr[CInt],
    writer: Ptr[stdio.FILE],
    settings: Settings,
  ): IO[Unit] =
    IO {
      Zone {
        /* libcurl copies the string options and CURLOPT_COPYPOSTFIELDS, so the
         * Zone memory is not used after this block. CURLOPT_POSTFIELDSIZE must
         * come before CURLOPT_COPYPOSTFIELDS. */
        val bodyOptions: List[() => CInt] =
          if (body.isEmpty) {
            Nil
          } else {
            val bodyPtr = alloc[Byte](body.length.toUSize)
            string.memcpy(bodyPtr, body.at(0), body.length.toUSize): Unit
            List(
              () => LibCurl.curl_easy_setopt(curl, CurlOpt.PostFieldSize, body.length.toLong),
              () => LibCurl.curl_easy_setopt(curl, CurlOpt.CopyPostFields, bodyPtr),
            )
          }
        /* Without CURLOPT_NOBODY, libcurl waits for a body of Content-Length
         * bytes after the headers of a HEAD response, and fails with code 18
         * or waits until the idle limit. */
        val headOptions: List[() => CInt] =
          if (request.method === Method.HEAD) List(() => LibCurl.curl_easy_setopt(curl, CurlOpt.NoBody, 1L))
          else Nil
        val options: List[() => CInt]     = List(
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.Url, toCString(request.uri.renderString)),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.CustomRequest, toCString(request.method.name)),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.Header, 1L),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.HttpHeader, list),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.WriteData, writer),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.NoSignal, 1L),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.ConnectTimeout, settings.connectTimeout.toSeconds),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.LowSpeedLimit, 1L),
          () => LibCurl.curl_easy_setopt(curl, CurlOpt.LowSpeedTime, settings.idleTimeout.toSeconds),
          () => CurlTransferNative.claudeproxymate_curl_use_abort_flag(curl, flag),
        )
        (options ++ headOptions ++ bodyOptions).collectFirstSome { option =>
          val code = option()
          Option.when(code =!= CurlCode.Ok)(code)
        }
      }
    }.flatMap(_.traverse_(code => IO.raiseError(ProxyErrorException(ProxyError.CurlOptionFailed(code)))))

  /* Runs on a blocking thread. errno is per thread, so it is read here. */
  private def readPipe(readFd: CInt, buffer: Ptr[Byte], bufferBytes: Int): ReadResult = {
    val n = unistd.read(readFd, buffer, bufferBytes.toUSize)
    if (n > 0) {
      val bytes = new Array[Byte](n)
      string.memcpy(bytes.at(0), buffer, n.toUSize): Unit
      ReadResult.Bytes(Chunk.array(bytes))
    } else if (n === 0) {
      ReadResult.EndOfFile
    } else {
      ReadResult.Failed(libcErrno.errno)
    }
  }

  /** One read from the pipe.
    *
    * `IO.blocking` cannot be cancelled, so a cancel would wait until data
    * comes. So the read runs on its own fiber, and only the join is cancelled.
    * On cancel, the abort flag makes libcurl stop and close the write end, so
    * the read ends soon. The fiber stays in `inFlight` until it ends, and the
    * release waits for it before it closes the read end and frees the buffer.
    */
  private def readOnce(
    readFd: CInt,
    buffer: Ptr[Byte],
    bufferBytes: Int,
    flag: Ptr[CInt],
    inFlight: Ref[IO, Option[FiberIO[ReadResult]]],
  ): IO[ReadResult] =
    IO.uncancelable { poll =>
      IO.blocking(readPipe(readFd, buffer, bufferBytes))
        .start
        .flatTap(read => inFlight.set(read.some))
        .flatMap { read =>
          poll(read.joinWithNever)
            .onCancel(IO(CurlTransferNative.claudeproxymate_curl_abort(flag)))
            .flatTap(_ => inFlight.set(none[FiberIO[ReadResult]]))
        }
    }

  private def nextChunk(read: IO[ReadResult]): IO[Option[Chunk[Byte]]] =
    read.flatMap {
      case ReadResult.Bytes(chunk) =>
        IO.pure(chunk.some)
      case ReadResult.EndOfFile =>
        IO.pure(none[Chunk[Byte]])
      case ReadResult.Failed(errno) if errno === posixErrno.EINTR =>
        nextChunk(read)
      case ReadResult.Failed(errno) =>
        IO.raiseError(new java.io.IOException(s"read() from the libcurl pipe failed with errno $errno"))
    }

  private def readHead(next: IO[Option[Chunk[Byte]]], maxHeadBytes: Int, received: Chunk[Byte]): IO[Head] =
    next.flatMap {
      case Some(chunk) =>
        val all = received ++ chunk
        UpstreamHead.parse(all.toArray, maxHeadBytes) match {
          case UpstreamHeadParse.NeedMore => readHead(next, maxHeadBytes, all)
          case UpstreamHeadParse.Parsed(head, bodyStart) => IO.pure(Head.Complete(head, all.drop(bodyStart)))
          case UpstreamHeadParse.Failed(error) => IO.pure(Head.Invalid(error))
        }
      case None =>
        IO.pure(Head.EndedEarly)
    }

  private def respond(
    run: Running,
    buffer: Ptr[Byte],
    flag: Ptr[CInt],
    inFlight: Ref[IO, Option[FiberIO[ReadResult]]],
    settings: Settings,
  ): IO[Response[IO]] = {
    val next = nextChunk(readOnce(run.readFd, buffer, settings.readBufferBytes, flag, inFlight))
    readHead(next, settings.maxHeadBytes, Chunk.empty[Byte]).flatMap {
      case Head.Complete(head, rest) =>
        Status.fromInt(head.status) match {
          case Right(status) =>
            IO.pure(
              Response[IO](status)
                .withHeaders(
                  Headers(
                    head
                      .headers
                      .map(h => Header.Raw(CIString(h.name), h.value))
                      .filterNot(h => HopByHopResponseHeaders.contains(h.name))
                  )
                )
                .withBodyStream(
                  Stream.chunk(rest) ++
                    Stream.repeatEval(next).unNoneTerminate.flatMap(Stream.chunk) ++
                    Stream.exec(checkPerform(run.perform))
                )
            )
          case Left(_) =>
            malformed(s"status ${head.status} is not a valid HTTP status")
        }
      case Head.Invalid(error) =>
        malformed(error.show)
      case Head.EndedEarly =>
        run.perform.joinWithNever.flatMap { code =>
          if (code =!= CurlCode.Ok) IO.pure(ProxyErrorHttp4s.asResponse(ProxyError.CurlPerformFailed(code)))
          else malformed("the response ended before its headers")
        }
    }
  }

  /** After the end of the body: a transfer that libcurl stopped with an error
    * (for example 28 for the idle limit) fails the stream, so the client sees
    * an incomplete response, not a complete one.
    */
  private def checkPerform(perform: FiberIO[CInt]): IO[Unit] =
    perform.joinWithNever.flatMap { code =>
      IO.raiseWhen(code =!= CurlCode.Ok)(ProxyErrorException(ProxyError.CurlPerformFailed(code)))
    }

  private def malformed(detail: String): IO[Response[IO]] =
    IO(System.err.println(s"claude-proxymate: malformed upstream response: $detail"))
      .as(ProxyErrorHttp4s.asResponse(ProxyError.MalformedUpstreamResponse))
}
