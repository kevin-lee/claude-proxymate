package claudeproxymate.proxy

import cats.effect.IO

/** Ties this process's lifetime to the parent that holds its stdin.
  *
  * The Electron main process spawns the proxy with `stdio: ["pipe", ...]` and
  * never writes to it, so the read below blocks for as long as the parent
  * lives. Every way the parent can die - Cmd+Q, Force Quit, SIGKILL, a crash -
  * closes the write end, and the read then returns -1. Reacting to that,
  * rather than waiting for a signal the parent may never get the chance to
  * send, is what stops the binary outliving the desktop app.
  *
  * The exit is immediate rather than a graceful shutdown, on purpose. With the
  * parent gone there is nobody left to serve, and the graceful path is exactly
  * what can stall: it waits on the in-flight `curl_easy_perform`, which
  * `CurlHttpClient` caps at `CURLOPT_TIMEOUT` (300s).
  */
object ParentWatch {

  def run(parentLink: ParentLink): IO[Unit] =
    parentLink match {
      case ParentLink.WatchStdin =>
        IO.blocking(System.in.read()).iterateWhile(_ >= 0) *> IO(System.exit(0))
      case ParentLink.Detached => IO.never[Unit]
    }
}
