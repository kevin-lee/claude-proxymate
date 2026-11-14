package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object ProxyExitSpec extends Properties {

  override def tests: List[Test] = List(
    property("Stopping and Idle give Stopped", testNotRunningGivesStopped),
    property("Running with exit code 0 and no signal gives Stopped", testCleanExitGivesStopped),
    property("an unexpected exit gives StartFailed before start and Crashed after start", testUnexpectedExit),
    example("describe gives the exit code when there is no stderr", testDescribeCodeOnly),
    example("describe gives the signal and the non-blank stderr lines", testDescribeSignalAndLines),
    example("describe keeps only the last MaxStderrLines lines", testDescribeKeepsLastLines),
    example("describe gives unknown exit without a code and a signal", testDescribeUnknownExit),
    example("describe gives the signal before the exit code", testDescribeSignalFirst),
    example("describe removes the \\r of \\r\\n line ends", testDescribeCrLf),
    example("Running with exit code 0 and a signal is not Stopped", testCodeZeroWithSignal),
    property("appendStderr keeps a suffix of at most MaxStderrChars characters", testAppendStderr),
  )

  private def genCode: Gen[Option[Int]] =
    Gen.int(Range.linear(-1, 255)).option

  private def genSignal: Gen[Option[String]] =
    Gen.element1("SIGABRT", "SIGSEGV", "SIGKILL", "SIGTERM").option

  private def genStderr: Gen[String] =
    Gen.string(Gen.element1('a', 'b', ' ', '\n'), Range.linear(0, 200))

  def testNotRunningGivesStopped: Property =
    for {
      lifecycle <- Gen.element1(ProxyLifecycle.Stopping, ProxyLifecycle.Idle).log("lifecycle")
      started   <- Gen.boolean.log("started")
      code      <- genCode.log("code")
      signal    <- genSignal.log("signal")
      stderr    <- genStderr.log("stderr")
    } yield {
      ProxyExit.classify(lifecycle, started, code, signal, stderr) ==== ProxyExitOutcome.Stopped
    }

  def testCleanExitGivesStopped: Property =
    for {
      started <- Gen.boolean.log("started")
      stderr  <- genStderr.log("stderr")
    } yield {
      ProxyExit.classify(ProxyLifecycle.Running, started, Some(0), None, stderr) ==== ProxyExitOutcome.Stopped
    }

  def testUnexpectedExit: Property =
    for {
      started <- Gen.boolean.log("started")
      exit    <- Gen
                   .choice1(
                     Gen.int(Range.linear(1, 255)).map(n => (Option(n), Option.empty[String])),
                     genSignal.map(s => (Option.empty[Int], s.orElse(Some("SIGABRT")))),
                   )
                   .log("exit")
      stderr  <- genStderr.log("stderr")
    } yield {
      val (code, signal) = exit
      val detail         = ProxyExit.describe(code, signal, stderr)
      val expected       =
        if (started) ProxyExitOutcome.Crashed(detail) else ProxyExitOutcome.StartFailed(detail)
      ProxyExit.classify(ProxyLifecycle.Running, started, code, signal, stderr) ==== expected
    }

  def testDescribeCodeOnly: Result =
    ProxyExit.describe(Some(134), None, "") ==== "exit code 134"

  def testDescribeSignalAndLines: Result =
    ProxyExit.describe(None, Some("SIGABRT"), "a\n\nb\n") ==== "signal SIGABRT\na\nb"

  def testDescribeKeepsLastLines: Result = {
    val stderr   = (1 to 30).map(n => s"line $n").mkString("\n")
    val expected = ("exit code 1" :: (11 to 30).map(n => s"line $n").toList).mkString("\n")
    ProxyExit.describe(Some(1), None, stderr) ==== expected
  }

  def testDescribeUnknownExit: Result =
    ProxyExit.describe(None, None, "") ==== "unknown exit"

  def testDescribeSignalFirst: Result =
    ProxyExit.describe(Some(1), Some("SIGKILL"), "") ==== "signal SIGKILL"

  def testDescribeCrLf: Result =
    ProxyExit.describe(Some(1), None, "a\r\nb\r\n") ==== "exit code 1\na\nb"

  def testCodeZeroWithSignal: Result =
    ProxyExit.classify(ProxyLifecycle.Running, false, Some(0), Some("SIGTERM"), "") ====
      ProxyExitOutcome.StartFailed("signal SIGTERM")

  /* A string of the given length from a repeating alphabet, starting at the given
   * offset. Gen.string with thousands of characters overflows the stack on
   * Scala.js, and the strings must be longer than MaxStderrChars. */
  private def genLongString(maxLength: Int): Gen[String] =
    for {
      length <- Gen.int(Range.linear(0, maxLength))
      offset <- Gen.int(Range.linear(0, 25))
    } yield (0 until length).map(i => ('a' + (offset + i) % 26).toChar).mkString

  def testAppendStderr: Property =
    for {
      current <- genLongString(ProxyExit.MaxStderrChars).log("current")
      chunk   <- genLongString(2 * ProxyExit.MaxStderrChars).log("chunk")
    } yield {
      val result = ProxyExit.appendStderr(current, chunk)
      Result.all(
        List(
          Result.assert(result.length <= ProxyExit.MaxStderrChars).log(s"length ${result.length.toString}"),
          Result.assert((current + chunk).endsWith(result)).log("not a suffix of current + chunk"),
        )
      )
    }
}
