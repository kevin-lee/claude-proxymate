package claudeproxymate.proxy

import cats.effect.IO
import claudeproxymate.core.{JsonLineProtocol, ProxyEvent}

/** Emits ProxyEvent as JSON lines to stdout for the Electron main process to read. */
object EventEmitter {

  def emit(event: ProxyEvent): IO[Unit] = IO {
    val line = JsonLineProtocol.encode(event)
    System.out.println(line)
    System.out.flush()
  }
}
