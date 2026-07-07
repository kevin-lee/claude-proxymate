package claudeproxymate.core

/* munit port of the shared hedgehog CorrelationIdsSpec for Scala Native,
 * where the hedgehog runner hangs. Every hedgehog example is ported 1:1.
 */
class CorrelationIdsSpec extends munit.FunSuite {

  private def hasMatch(s: String, name: String): Boolean =
    CorrelationIds.scan(s).exists(_.name == name)

  /* ── Positive ─────────────────────────────────────────────────────────── */

  test("msg_ matches with 16+ char body") {
    assert(hasMatch("msg_01ABCDEFGHIJKLMNOP", "msg"), "expected msg match")
  }

  test("toolu_ matches with 16+ char body") {
    assert(hasMatch("toolu_abcdefghijklmnopqrst", "toolu"), "expected toolu match")
  }

  test("srvtoolu_ matches with 16+ char body") {
    assert(hasMatch("srvtoolu_abcdefghijklmnopqrst", "srvtoolu"), "expected srvtoolu match")
  }

  /* ── Negative ─────────────────────────────────────────────────────────── */

  test("msg_short does NOT match (below 16-char floor)") {
    assertEquals(CorrelationIds.scan("msg_short"), List.empty[CorrelationIds.CorrMatch])
  }

  test("bare 'msg' does NOT match (no underscore + body)") {
    assertEquals(CorrelationIds.scan("msg"), List.empty[CorrelationIds.CorrMatch])
  }

  test("plain integer-like '42' does NOT match") {
    assertEquals(CorrelationIds.scan("42"), List.empty[CorrelationIds.CorrMatch])
  }

  /* ── Overlap ──────────────────────────────────────────────────────────── */

  test("srvtoolu_ wins over toolu_ when prefixes overlap") {
    val ms = CorrelationIds.scan("srvtoolu_abcdefghijklmnopqrst")
    assertEquals(ms.map(_.name), List("srvtoolu"))
  }

  /* ── Multiple matches ─────────────────────────────────────────────────── */

  test("multiple ids in one string returned in start order") {
    val s  = "msg_01ABCDEFGHIJKLMNOPQR and toolu_abcdefghijklmnopqrst"
    val ms = CorrelationIds.scan(s)
    ms match {
      case first :: second :: Nil =>
        assert(first.start < second.start, s"order wrong: $ms")
        assertEquals(first.name, "msg")
      case other =>
        fail(s"expected 2 matches, got $other")
    }
  }

  /* ── Disjoint surface with TokenPatterns ──────────────────────────────── */

  test("scan does not return TokenPatterns matches") {
    val s  = "key=sk-ant-fakeABCDEFGHIJKLMNOPQRSTUVWXYZ id=msg_01ABCDEFGHIJKLMNOPQR"
    val cs = CorrelationIds.scan(s)
    assert(cs.exists(_.name == "msg"), s"msg id missing: $cs")
    assert(!cs.exists(_.name.startsWith("anthropic")), s"unexpected token match in corr scan: $cs")
  }

  /* ── Fingerprint ──────────────────────────────────────────────────────── */

  test("fingerprint(msg) prefix-preserves and emits last-4") {
    val raw = "msg_01ABCDEFGHIJKLMNOPQRSTUVWXYZ4"
    assertEquals(CorrelationIds.fingerprint("msg", raw), "msg_…XYZ4")
  }

  test("fingerprint(toolu) prefix-preserves and emits last-4") {
    val raw = "toolu_abcdefghijklmnopqrstuvwxyz"
    assertEquals(CorrelationIds.fingerprint("toolu", raw), "toolu_…wxyz")
  }

  test("fingerprint(srvtoolu) prefix-preserves and emits last-4") {
    val raw = "srvtoolu_zyxwvutsrqponmlkjihgfedcba"
    assertEquals(CorrelationIds.fingerprint("srvtoolu", raw), "srvtoolu_…dcba")
  }

  test("fingerprint when raw is too short returns raw verbatim") {
    /* raw is only "msg_test" (length 8). Compaction would yield
     * "msg_…test" (length 9), longer than the input. Return raw.
     */
    val raw = "msg_test"
    assertEquals(CorrelationIds.fingerprint("msg", raw), raw)
  }
}
