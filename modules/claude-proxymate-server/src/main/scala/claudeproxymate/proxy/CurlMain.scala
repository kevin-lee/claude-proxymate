package claudeproxymate.proxy

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.IORuntimeConfig
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import claudeproxymate.core.ProxyEvent

import scala.concurrent.duration.*

/** Entry point using libcurl for HTTPS (no s2n dependency needed).
  *
  * Uses the standard IOApp runtime (EmberServer works) with a synchronous
  * libcurl-based HTTP client for outbound HTTPS to api.anthropic.com.
  *
  * Switch via build.sbt:
  *   Compile / mainClass := Some("claudeproxymate.proxy.CurlMain")  // libcurl (default)
  *   Compile / mainClass := Some("claudeproxymate.proxy.Main")      // s2n
  */
object CurlMain extends IOApp {

  /* Upper bound on how long SIGTERM/SIGINT may take to actually end the
   * process. Cats Effect already has the hard-exit path
   * (`IO.sleep(shutdownHookTimeout) *> System.exit`), but its default is
   * `Duration.Inf`, so the fallback never fires: a SIGTERM arriving while
   * `CurlHttpClient`'s uncancelable `IO.blocking(curl_easy_perform)` is in
   * flight leaves the graceful shutdown parked behind it for as long as
   * `CURLOPT_TIMEOUT` (300s) allows. A finite value arms the fallback. */
  private val ShutdownDeadline: FiniteDuration = 5.seconds

  override protected def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(shutdownHookTimeout = ShutdownDeadline)

  override def run(args: List[String]): IO[ExitCode] = {
    val port             = Port.fromInt(PortArg.parse(args)).getOrElse(port"8888")
    val filterConfigPath = FilterConfigArg.parse(args)
    val parentLink       = ParentLinkArg.parse(args)

    FilterConfigLoader
      .make(filterConfigPath)
      .flatMap { loader =>
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"127.0.0.1")
          .withPort(port)
          .withHttpApp(ProxyServer.routes(CurlHttpClient.client, loader))
          .build
          .use { server =>
            for {
              _ <- EventEmitter.emit(ProxyEvent.ProxyStarted(server.address.port.value))
              /* `.start`, never `.background`: a joined finalizer would wait
               * on the uncancelable stdin read at shutdown and stall every
               * SIGTERM until ShutdownDeadline. */
              _ <- ParentWatch.run(parentLink).start
              _ <- IO.never[Unit]
            } yield ()
          }
      }
      .handleErrorWith { e =>
        /* Bind/startup failures must reach the Electron main process as a
         * protocol event (stdout); the stack trace still goes to stderr. */
        EventEmitter.emit(ProxyEvent.ProxyError(Option(e.getMessage).getOrElse(e.getClass.getName))) *>
          IO.raiseError(e)
      }
      .as(ExitCode.Success)
  }
}
