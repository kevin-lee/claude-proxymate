package claudeproxymate.core

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object ClaudeEnvSpec extends Properties {

  override def tests: List[Test] = List(
    property("no env.ANTHROPIC_BASE_URL value => Set", testAbsentSets),
    property("value already at baseUrl(port) => AlreadyApplied", testSameValueAlreadyApplied),
    property("owned value on a different port => Set", testOwnedDifferentPortSets),
    property("foreign value => SkipForeign and never removed", testForeignSkippedAndNeverRemoved),
    property("decideRemove with no record and no fallback => NoOp", testRemoveWithoutRecordOrFallbackNoOp),
    property("apply-then-remove round-trip: recorded value is removable", testApplyRemoveRoundTrip),
    property("absent value is never removed", testAbsentNeverRemoved),
    example("recorded value is removable independent of the fallback", testRecordedValueRemovable),
    example("fallback removal requires a local proxy URL", testFallbackRequiresLocalProxyUrl),
  )

  private def genPort: Gen[Int] = Gen.int(Range.linear(1024, 65535))

  private def genForeignUrl: Gen[String] =
    Gen.choice1(
      Gen.constant("https://my-gateway.example.com"),
      Gen.constant("http://gateway.internal:8080"),
      Gen.constant("http://localhost:8888/v1"),
      Gen.string(Gen.alphaNum, Range.linear(1, 20)).map(s => s"https://$s.example.com"),
    )

  def testAbsentSets: Property =
    for {
      port <- genPort.log("port")
    } yield {
      ClaudeEnv.decideApply(none[String], port, none[String]) ==== ClaudeEnv.ApplyDecision.Set
    }

  def testSameValueAlreadyApplied: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val current = VsCodeEnv.baseUrl(port).some
      ClaudeEnv.decideApply(current, port, none[String]) ==== ClaudeEnv.ApplyDecision.AlreadyApplied
    }

  def testOwnedDifferentPortSets: Property =
    for {
      port  <- genPort.log("port")
      delta <- Gen.int(Range.linear(1, 100)).log("delta")
    } yield {
      val oldPort = if (port + delta <= 65535) port + delta else port - delta
      val current = VsCodeEnv.baseUrl(oldPort).some
      ClaudeEnv.decideApply(current, port, none[String]) ==== ClaudeEnv.ApplyDecision.Set
    }

  def testForeignSkippedAndNeverRemoved: Property =
    for {
      port    <- genPort.log("port")
      foreign <- genForeignUrl.log("foreign")
    } yield {
      val applyResult  = ClaudeEnv.decideApply(foreign.some, port, none[String])
      val removeResult = ClaudeEnv.decideRemove(foreign.some, none[String], VsCodeEnv.baseUrl(port).some)
      Result.all(
        List(
          applyResult ==== ClaudeEnv.ApplyDecision.SkipForeign(foreign),
          removeResult ==== ClaudeEnv.RemoveDecision.NoOp,
        )
      )
    }

  def testRemoveWithoutRecordOrFallbackNoOp: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val current = VsCodeEnv.baseUrl(port).some
      ClaudeEnv.decideRemove(current, none[String], none[String]) ==== ClaudeEnv.RemoveDecision.NoOp
    }

  def testApplyRemoveRoundTrip: Property =
    for {
      port <- genPort.log("port")
    } yield {
      /* Simulate: Set decided, value written and recorded, then remove
       * with the record. */
      val url = VsCodeEnv.baseUrl(port)
      ClaudeEnv.decideRemove(url.some, url.some, none[String]) ==== ClaudeEnv.RemoveDecision.Remove
    }

  def testAbsentNeverRemoved: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val url = VsCodeEnv.baseUrl(port)
      ClaudeEnv.decideRemove(none[String], url.some, url.some) ==== ClaudeEnv.RemoveDecision.NoOp
    }

  def testRecordedValueRemovable: Result = {
    /* The record is authoritative: whatever the record says we wrote is
     * removable, independent of the fallback pattern. */
    val recorded = "http://localhost:8888"
    ClaudeEnv.decideRemove(recorded.some, recorded.some, none[String]) ==== ClaudeEnv.RemoveDecision.Remove
  }

  def testFallbackRequiresLocalProxyUrl: Result = {
    /* With no record, the fallback match alone is not enough — the
     * value must also look like a local proxy URL. */
    val gateway = "https://my-gateway.example.com"
    Result.all(
      List(
        ClaudeEnv.decideRemove(gateway.some, none[String], gateway.some) ==== ClaudeEnv.RemoveDecision.NoOp,
        ClaudeEnv.decideRemove(
          "http://localhost:9999".some,
          none[String],
          "http://localhost:9999".some,
        ) ==== ClaudeEnv.RemoveDecision.Remove,
      )
    )
  }
}
