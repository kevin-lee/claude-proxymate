package claudeproxymate.proxy

import cats.syntax.all.*

import scala.annotation.tailrec

/** `--filter-config <path>` command-line parsing, shared by [[CurlMain]] and [[Main]].
  *
  * Same shape as [[PortArg]]: the args must come from `IOApp.run(args)` because
  * JVM-only properties are absent on Scala Native.
  */
object FilterConfigArg {

  @tailrec
  def parse(args: List[String]): Option[String] =
    args match {
      case "--filter-config" :: path :: _ => path.some
      case _ :: rest => parse(rest)
      case Nil => none[String]
    }
}
