package claudeproxymate.proxy

import cats.*
import cats.derived.strict.*

/** Whether this process is tied to the lifetime of a parent that holds its stdin.
  *
  *   - [[WatchStdin]]: the parent (the Electron main process) keeps a stdin
  *     pipe open and never writes to it. EOF on stdin therefore means the
  *     parent is gone - by any means, including SIGKILL, a crash, or Force
  *     Quit - and this process exits with it.
  *   - [[Detached]]: stdin is ignored. This is the default so the binary stays
  *     usable standalone from a terminal, where stdin may be a TTY,
  *     `/dev/null`, or closed, none of which mean "the parent is gone".
  */
enum ParentLink derives CanEqual, Eq, Hash {
  case WatchStdin
  case Detached
}
