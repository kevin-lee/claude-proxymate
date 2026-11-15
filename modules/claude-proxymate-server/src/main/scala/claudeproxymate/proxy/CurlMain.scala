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
   * `Duration.Inf`, so the fallback never fires. A cancelled transfer of
   * `CurlHttpClient` sets its abort flag, and libcurl stops within about one
   * second, but `IO.blocking(curl_easy_perform)` itself cannot be cancelled.
   * A finite value makes sure that a transfer that does not stop cannot hold
   * the shutdown. */
  private val ShutdownDeadline: FiniteDuration = 5.seconds

  override protected def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(shutdownHookTimeout = ShutdownDeadline)

  override def run(args: List[String]): IO[ExitCode] = {
    val port             = Port.fromInt(PortArg.parse(args)).getOrElse(port"8888")
    val filterConfigPath = FilterConfigArg.parse(args)
    val parentLink       = ParentLinkArg.parse(args)

    (CurlHttpClient.globalInit *> FilterConfigLoader.make(filterConfigPath))
      .flatMap { loader =>
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"127.0.0.1")
          .withPort(port)
          .withHttpApp(
            ProxyServer.routes(CurlHttpClient.client(CurlHttpClient.Settings.Default), Upstream.Anthropic, loader)
          )
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
