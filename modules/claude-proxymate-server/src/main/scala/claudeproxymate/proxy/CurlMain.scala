package claudeproxymate.proxy

import cats.effect.{IO, IOApp}
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
object CurlMain extends IOApp.Simple {

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
  }
}
