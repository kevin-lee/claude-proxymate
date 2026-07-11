package claudeproxymate.core

import cats.syntax.all.*
import hedgehog.*
import hedgehog.runner.*

object VsCodeEnvSpec extends Properties {

  override def tests: List[Test] = List(
    property("no ANTHROPIC_BASE_URL entry => Append", testAbsentAppends),
    property("entry already at baseUrl(port) => AlreadyApplied", testSameValueAlreadyApplied),
    property("owned entry on a different port => Update at its index", testOwnedDifferentPortUpdates),
    property("foreign value => SkipForeign and never removed", testForeignSkippedAndNeverRemoved),
    property("owned duplicates are all dropped on apply and all removed on remove", testDuplicatesHandled),
    property("decideRemove with no record and no fallback => NoOp", testRemoveWithoutRecordOrFallbackNoOp),
    property("apply-then-remove round-trip leaves no owned entries", testApplyRemoveRoundTrip),
    example("other env var names are ignored", testOtherNamesIgnored),
    example("recorded value is removable", testRecordedValueRemovable),
  )

  private def genPort: Gen[Int] = Gen.int(Range.linear(1024, 65535))

  private def genOtherEntry(index: Int): Gen[VsCodeEnv.EnvEntry] =
    for {
      name  <- Gen.string(Gen.alphaNum, Range.linear(1, 12)).map(s => s"OTHER_$s")
      value <- Gen.string(Gen.alphaNum, Range.linear(0, 12))
    } yield VsCodeEnv.EnvEntry(index, name, value)

  private def genForeignUrl: Gen[String] =
    Gen.choice1(
      Gen.constant("https://my-gateway.example.com"),
      Gen.constant("http://gateway.internal:8080"),
      Gen.constant("http://localhost:8888/v1"),
      Gen.string(Gen.alphaNum, Range.linear(1, 20)).map(s => s"https://$s.example.com"),
    )

  def testAbsentAppends: Property =
    for {
      port  <- genPort.log("port")
      other <- genOtherEntry(0).log("other")
    } yield {
      VsCodeEnv.decideApply(List(other), port, none[String]) ==== VsCodeEnv.ApplyDecision.Append
    }

  def testSameValueAlreadyApplied: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val entries = List(VsCodeEnv.EnvEntry(0, VsCodeEnv.EnvVarName, VsCodeEnv.baseUrl(port)))
      VsCodeEnv.decideApply(entries, port, none[String]) ==== VsCodeEnv.ApplyDecision.AlreadyApplied(Nil)
    }

  def testOwnedDifferentPortUpdates: Property =
    for {
      port  <- genPort.log("port")
      delta <- Gen.int(Range.linear(1, 100)).log("delta")
      index <- Gen.int(Range.linear(0, 5)).log("index")
    } yield {
      val oldPort = if (port + delta <= 65535) port + delta else port - delta
      val entries = List(VsCodeEnv.EnvEntry(index, VsCodeEnv.EnvVarName, VsCodeEnv.baseUrl(oldPort)))
      VsCodeEnv.decideApply(entries, port, none[String]) ==== VsCodeEnv.ApplyDecision.Update(index, Nil)
    }

  def testForeignSkippedAndNeverRemoved: Property =
    for {
      port    <- genPort.log("port")
      foreign <- genForeignUrl.log("foreign")
    } yield {
      val entries      = List(VsCodeEnv.EnvEntry(0, VsCodeEnv.EnvVarName, foreign))
      val applyResult  = VsCodeEnv.decideApply(entries, port, none[String])
      val removeResult = VsCodeEnv.decideRemove(entries, none[String], VsCodeEnv.baseUrl(port).some)
      Result.all(
        List(
          applyResult ==== VsCodeEnv.ApplyDecision.SkipForeign(List(foreign)),
          removeResult ==== VsCodeEnv.RemoveDecision.NoOp,
        )
      )
    }

  def testDuplicatesHandled: Property =
    for {
      port  <- genPort.log("port")
      port2 <- genPort.log("port2")
    } yield {
      val url     = VsCodeEnv.baseUrl(port)
      val entries = List(
        VsCodeEnv.EnvEntry(0, VsCodeEnv.EnvVarName, url),
        VsCodeEnv.EnvEntry(1, VsCodeEnv.EnvVarName, VsCodeEnv.baseUrl(port2)),
        VsCodeEnv.EnvEntry(2, VsCodeEnv.EnvVarName, url),
      )
      val applied = VsCodeEnv.decideApply(entries, port, url.some)
      val removed = VsCodeEnv.decideRemove(entries, url.some, none[String])
      Result.all(
        List(
          applied ==== VsCodeEnv.ApplyDecision.AlreadyApplied(List(1, 2)),
          removed ==== VsCodeEnv.RemoveDecision.Remove(List(0, 2)),
        )
      )
    }

  def testRemoveWithoutRecordOrFallbackNoOp: Property =
    for {
      port <- genPort.log("port")
    } yield {
      val entries = List(VsCodeEnv.EnvEntry(0, VsCodeEnv.EnvVarName, VsCodeEnv.baseUrl(port)))
      VsCodeEnv.decideRemove(entries, none[String], none[String]) ==== VsCodeEnv.RemoveDecision.NoOp
    }

  def testApplyRemoveRoundTrip: Property =
    for {
      port  <- genPort.log("port")
      other <- genOtherEntry(1).log("other")
    } yield {
      val url     = VsCodeEnv.baseUrl(port)
      /* Simulate: Append decided, entry written at index 0, then remove with the record. */
      val entries = List(VsCodeEnv.EnvEntry(0, VsCodeEnv.EnvVarName, url), other)
      VsCodeEnv.decideRemove(entries, url.some, none[String]) match {
        case VsCodeEnv.RemoveDecision.Remove(indices) =>
          val survivors = entries.filterNot(entry => indices.contains(entry.index))
          Result.assert(survivors.forall(entry => entry.name =!= VsCodeEnv.EnvVarName)).log(s"survivors=$survivors")
        case VsCodeEnv.RemoveDecision.NoOp =>
          Result.failure.log("expected Remove, got NoOp")
      }
    }

  def testOtherNamesIgnored: Result = {
    val entries = List(
      VsCodeEnv.EnvEntry(0, "ANTHROPIC_AUTH_TOKEN", "sk-something"),
      VsCodeEnv.EnvEntry(1, "HTTP_PROXY", "http://localhost:3128"),
    )
    Result.all(
      List(
        VsCodeEnv.decideApply(entries, 8888, none[String]) ==== VsCodeEnv.ApplyDecision.Append,
        VsCodeEnv.decideRemove(entries, "http://localhost:8888".some, none[String]) ==== VsCodeEnv.RemoveDecision.NoOp,
      )
    )
  }

  def testRecordedValueRemovable: Result = {
    /* The record is authoritative: whatever the record says we wrote is
     * removable, independent of the fallback pattern. */
    val recorded = "http://localhost:8888"
    val entries  = List(VsCodeEnv.EnvEntry(3, VsCodeEnv.EnvVarName, recorded))
    VsCodeEnv.decideRemove(entries, recorded.some, none[String]) ==== VsCodeEnv.RemoveDecision.Remove(List(3))
  }
}
