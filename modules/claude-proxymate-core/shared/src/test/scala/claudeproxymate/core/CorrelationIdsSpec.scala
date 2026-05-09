package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object CorrelationIdsSpec extends Properties {

  override def tests: List[Test] = List(
    // Per-pattern positive
    example("msg_ matches with 16+ char body", testMsgMatch),
    example("toolu_ matches with 16+ char body", testTooluMatch),
    example("srvtoolu_ matches with 16+ char body", testSrvtooluMatch),
    // Negative
    example("msg_short does NOT match (below 16-char floor)", testMsgShortNoMatch),
    example("bare 'msg' does NOT match (no underscore + body)", testMsgBareNoMatch),
    example("plain integer-like '42' does NOT match", testIntegerNoMatch),
    // Overlap
    example("srvtoolu_ wins over toolu_ when prefixes overlap", testSrvtooluOverToolu),
    // Multiple matches
    example("multiple ids in one string returned in start order", testMultipleInOrder),
    // Disjoint with TokenPatterns
    example("scan does not return TokenPatterns matches", testDisjointWithToken),
    // Fingerprint
    example("fingerprint(msg) prefix-preserves and emits last-4", testFingerprintMsg),
    example("fingerprint(toolu) prefix-preserves and emits last-4", testFingerprintToolu),
    example("fingerprint(srvtoolu) prefix-preserves and emits last-4", testFingerprintSrvtoolu),
    example("fingerprint when raw is too short returns raw verbatim", testFingerprintShortReturnsRaw),
  )

  private def hasMatch(s: String, name: String): Boolean =
    CorrelationIds.scan(s).exists(_.name == name)

  // ── Positive ────────────────────────────────────────────────────────────

  def testMsgMatch: Result =
    Result.assert(hasMatch("msg_01ABCDEFGHIJKLMNOP", "msg"))
      .log("expected msg match")

  def testTooluMatch: Result =
    Result.assert(hasMatch("toolu_abcdefghijklmnopqrst", "toolu"))
      .log("expected toolu match")

  def testSrvtooluMatch: Result =
    Result.assert(hasMatch("srvtoolu_abcdefghijklmnopqrst", "srvtoolu"))
      .log("expected srvtoolu match")

  // ── Negative ────────────────────────────────────────────────────────────

  def testMsgShortNoMatch: Result =
    Result.assert(CorrelationIds.scan("msg_short").isEmpty)
      .log("msg_ below 16-char floor should not match")

  def testMsgBareNoMatch: Result =
    Result.assert(CorrelationIds.scan("msg").isEmpty)
      .log("bare 'msg' should not match")

  def testIntegerNoMatch: Result =
    Result.assert(CorrelationIds.scan("42").isEmpty)
      .log("plain integer should not match")

  // ── Overlap ─────────────────────────────────────────────────────────────

  def testSrvtooluOverToolu: Result = {
    val s  = "srvtoolu_abcdefghijklmnopqrst"
    val ms = CorrelationIds.scan(s)
    Result.all(
      List(
        Result.assert(ms.size == 1).log(s"expected 1 match, got $ms"),
        Result.assert(ms.headOption.exists(_.name == "srvtoolu"))
          .log(s"expected srvtoolu, got $ms"),
      )
    )
  }

  def testMultipleInOrder: Result = {
    val s  = "msg_01ABCDEFGHIJKLMNOPQR and toolu_abcdefghijklmnopqrst"
    val ms = CorrelationIds.scan(s)
    Result.all(
      List(
        Result.assert(ms.size == 2).log(s"expected 2 matches, got $ms"),
        Result.assert(ms.head.start < ms(1).start).log(s"order wrong: $ms"),
        Result.assert(ms.head.name == "msg").log(s"first should be msg, got $ms"),
      )
    )
  }

  // ── Disjoint surface with TokenPatterns ────────────────────────────────

  def testDisjointWithToken: Result = {
    val s  = "key=sk-ant-fakeABCDEFGHIJKLMNOPQRSTUVWXYZ id=msg_01ABCDEFGHIJKLMNOPQR"
    val cs = CorrelationIds.scan(s)
    Result.all(
      List(
        Result.assert(cs.exists(_.name == "msg")).log(s"msg id missing: $cs"),
        Result.assert(!cs.exists(_.name.startsWith("anthropic"))).log(s"unexpected token match in corr scan: $cs"),
      )
    )
  }

  // ── Fingerprint ─────────────────────────────────────────────────────────

  def testFingerprintMsg: Result = {
    val raw = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ4"
    val fp  = CorrelationIds.fingerprint("msg", raw)
    Result.assert(fp == "msg_…WXYZ4".take(fp.length) || fp.startsWith("msg_…") && fp.endsWith("XYZ4"))
      .log(s"unexpected fingerprint: $fp")
  }

  def testFingerprintToolu: Result = {
    val raw = "toolu_abcdefghijklmnopqrstuvwxyz"
    val fp  = CorrelationIds.fingerprint("toolu", raw)
    Result.all(
      List(
        Result.assert(fp.startsWith("toolu_…")).log(s"prefix not preserved: $fp"),
        Result.assert(fp.endsWith("wxyz")).log(s"last-4 wrong: $fp"),
      )
    )
  }

  def testFingerprintSrvtoolu: Result = {
    val raw = "srvtoolu_zyxwvutsrqponmlkjihgfedcba"
    val fp  = CorrelationIds.fingerprint("srvtoolu", raw)
    Result.all(
      List(
        Result.assert(fp.startsWith("srvtoolu_…")).log(s"prefix not preserved: $fp"),
        Result.assert(fp.endsWith("dcba")).log(s"last-4 wrong: $fp"),
      )
    )
  }

  def testFingerprintShortReturnsRaw: Result = {
    // raw is only "msg_test" (length 8). Compaction would yield
    // "msg_…test" (length 9), longer than the input. Return raw.
    val raw = "msg_test"
    val fp  = CorrelationIds.fingerprint("msg", raw)
    Result.assert(fp == raw)
      .log(s"expected raw, got $fp")
  }
}
