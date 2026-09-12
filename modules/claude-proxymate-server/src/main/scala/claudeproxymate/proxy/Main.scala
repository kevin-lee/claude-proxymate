package claudeproxymate.proxy

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.IORuntimeConfig
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.ember.client.EmberClientBuilder
import claudeproxymate.core.ProxyEvent

import scala.concurrent.duration.*

object Main extends IOApp {

  /* Upper bound on how long SIGTERM/SIGINT may take to actually end the
   * process. Cats Effect already has the hard-exit path
   * (`IO.sleep(shutdownHookTimeout) *> System.exit`), but its default is
   * `Duration.Inf`, so the fallback never fires and a shutdown can stay
   * parked behind an in-flight request. A finite value arms the fallback.
   * Kept in lockstep with [[CurlMain]]. */
  private val ShutdownDeadline: FiniteDuration = 5.seconds

  override protected def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(shutdownHookTimeout = ShutdownDeadline)

  override def run(args: List[String]): IO[ExitCode] = {
    val port             = Port.fromInt(PortArg.parse(args)).getOrElse(port"8888")
    val filterConfigPath = FilterConfigArg.parse(args)
    val parentLink       = ParentLinkArg.parse(args)

    EmberClientBuilder
      .default[IO]
      .build
      .use { client =>
        FilterConfigLoader.make(filterConfigPath).flatMap { loader =>
          EmberServerBuilder
            .default[IO]
            .withHost(ipv4"127.0.0.1")
            .withPort(port)
            .withHttpApp(ProxyServer.routes(client, loader))
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
