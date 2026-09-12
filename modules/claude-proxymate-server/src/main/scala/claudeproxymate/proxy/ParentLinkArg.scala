package claudeproxymate.proxy

import scala.annotation.tailrec

/** `--exit-on-stdin-close` command-line parsing, shared by [[CurlMain]] and [[Main]].
  *
  * Same shape as [[PortArg]] and [[FilterConfigArg]]: the args must come from
  * `IOApp.run(args)` because JVM-only properties are absent on Scala Native.
  *
  * The flag is opt-in, and deliberately so. Without it the binary ignores
  * stdin entirely, which is what keeps standalone terminal use working: run
  * with `< /dev/null` an unconditional stdin watcher would see EOF at once and
  * exit immediately. Only the Electron main process, which owns the stdin pipe
  * it spawns the proxy with, passes the flag.
  */
object ParentLinkArg {

  val Flag: String = "--exit-on-stdin-close"

  @tailrec
  def parse(args: List[String]): ParentLink =
    args match {
      case `Flag` :: _ => ParentLink.WatchStdin
      case _ :: rest => parse(rest)
      case Nil => ParentLink.Detached
    }
}
