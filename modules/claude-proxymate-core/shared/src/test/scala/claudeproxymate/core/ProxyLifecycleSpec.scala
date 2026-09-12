package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object ProxyLifecycleSpec extends Properties {

  override def tests: List[Test] = List(
    property("a transition never yields the state it was given", testTransitionsAlwaysMove),
    property("reportsRunning is true for exactly Running", testReportsRunningOnlyRunning),
    example("onSpawn is allowed only from Idle", testSpawnOnlyFromIdle),
    example("onStopRequested is allowed only from Running", testStopOnlyFromRunning),
    example("a repeated stop while Stopping is refused", testRepeatStopRefused),
  )

  private def genLifecycle: Gen[ProxyLifecycle] =
    Gen.element1(ProxyLifecycle.Idle, ProxyLifecycle.Running, ProxyLifecycle.Stopping)

  /* Guards the escalation timers: a transition that could return its own
   * input would let a second stop re-arm the SIGKILL timeout. */
  def testTransitionsAlwaysMove: Property =
    for {
      current <- genLifecycle.log("current")
    } yield {
      Result.all(
        List(
          Result.assert(!ProxyLifecycle.onSpawn(current).contains(current)).log("onSpawn stayed put"),
          Result.assert(!ProxyLifecycle.onStopRequested(current).contains(current)).log("onStopRequested stayed put"),
        )
      )
    }

  def testReportsRunningOnlyRunning: Property =
    for {
      current <- genLifecycle.log("current")
    } yield {
      ProxyLifecycle.reportsRunning(current) ==== (current match {
        case ProxyLifecycle.Running => true
        case ProxyLifecycle.Idle | ProxyLifecycle.Stopping => false
      })
    }

  def testSpawnOnlyFromIdle: Result =
    Result.all(
      List(
        ProxyLifecycle.onSpawn(ProxyLifecycle.Idle) ==== Some(ProxyLifecycle.Running),
        ProxyLifecycle.onSpawn(ProxyLifecycle.Running) ==== None,
        ProxyLifecycle.onSpawn(ProxyLifecycle.Stopping) ==== None,
      )
    )

  def testStopOnlyFromRunning: Result =
    Result.all(
      List(
        ProxyLifecycle.onStopRequested(ProxyLifecycle.Running) ==== Some(ProxyLifecycle.Stopping),
        ProxyLifecycle.onStopRequested(ProxyLifecycle.Idle) ==== None,
        ProxyLifecycle.onStopRequested(ProxyLifecycle.Stopping) ==== None,
      )
    )

  def testRepeatStopRefused: Result = {
    val afterFirstStop = ProxyLifecycle.onStopRequested(ProxyLifecycle.Running)
    Result.all(
      List(
        afterFirstStop ==== Some(ProxyLifecycle.Stopping),
        afterFirstStop.flatMap(ProxyLifecycle.onStopRequested) ==== None,
      )
    )
  }
}
