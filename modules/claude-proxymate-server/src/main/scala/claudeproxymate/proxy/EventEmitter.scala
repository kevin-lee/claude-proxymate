package claudeproxymate.proxy

import cats.effect.IO
import claudeproxymate.core.{JsonLineProtocol, ProxyEvent}

/** Emits ProxyEvent as JSON lines to stdout for the Electron main process to read.
  *
  * IMPORTANT: this process's stdout IS the IPC protocol channel —
  * the Electron main process reads it line-by-line and decodes each
  * line as a `ProxyEvent` (see `IpcHandlers.processProxyEvent`).
  * Nothing else may write to stdout: any non-event line lands in the
  * middle of the event stream and fails to decode (it is dropped and
  * logged on the consumer side). Diagnostic / debug logging from the
  * proxy must go to `System.err`, never `System.out`.
  */
object EventEmitter {

  def emit(event: ProxyEvent): IO[Unit] = IO {
    val line = JsonLineProtocol.encode(event)
    System.out.println(line)
    System.out.flush()
  }
}
