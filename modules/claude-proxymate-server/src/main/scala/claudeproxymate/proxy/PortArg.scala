package claudeproxymate.proxy

/** Command-line port argument parsing, shared by [[CurlMain]] and [[Main]].
  *
  * Must run on Scala Native: the args come from `IOApp.run(args)`, never
  * from JVM-only properties like `sun.java.command` (absent on Native,
  * which silently disabled `--port` in earlier versions).
  */
object PortArg {

  val DefaultPort: Int = 8888

  def parse(args: List[String]): Int = {
    args match {
      case "--port" :: portStr :: _ =>
        portStr.toIntOption.filter(p => p >= 1024 && p <= 65535).getOrElse(DefaultPort)
      case _ :: rest => parse(rest)
      case Nil => DefaultPort
    }
  }
}
