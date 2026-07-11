package claudeproxymate.proxy

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import claudeproxymate.core.ProxyEvent

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

  override def run(args: List[String]): IO[ExitCode] = {
    val port = Port.fromInt(PortArg.parse(args)).getOrElse(port"8888")

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port)
      .withHttpApp(ProxyServer.routes(CurlHttpClient.client))
      .build
      .use { server =>
        for {
          _ <- EventEmitter.emit(ProxyEvent.ProxyStarted(server.address.port.value))
          _ <- IO.never[Unit]
        } yield ()
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
