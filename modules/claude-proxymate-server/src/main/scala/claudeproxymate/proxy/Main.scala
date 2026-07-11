package claudeproxymate.proxy

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import claudeproxymate.core.ProxyEvent

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val port = Port.fromInt(PortArg.parse(args)).getOrElse(port"8888")

    EmberClientBuilder
      .default[IO]
      .build
      .use { client =>
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"127.0.0.1")
          .withPort(port)
          .withHttpApp(ProxyServer.routes(client))
          .build
          .use { server =>
            for {
              _ <- EventEmitter.emit(ProxyEvent.ProxyStarted(server.address.port.value))
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
