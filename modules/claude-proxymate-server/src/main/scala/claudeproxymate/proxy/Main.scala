package claudeproxymate.proxy

import cats.effect.{IO, IOApp}
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import claudeproxymate.core.ProxyEvent

object Main extends IOApp.Simple {

  private def parsePort(args: List[String]): Int = {
    args match {
      case "--port" :: portStr :: _ =>
        portStr.toIntOption.filter(p => p >= 1024 && p <= 65535).getOrElse(8888)
      case _ :: rest => parsePort(rest)
      case Nil => 8888
    }
  }

  override def run: IO[Unit] = {
    val portNum = parsePort(
      sys.props.get("sun.java.command").map(_.split("\\s+").toList).getOrElse(Nil)
    )
    val port    = Port.fromInt(portNum).getOrElse(port"8888")

    EmberClientBuilder.default[IO].build.use { client =>
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
  }
}
