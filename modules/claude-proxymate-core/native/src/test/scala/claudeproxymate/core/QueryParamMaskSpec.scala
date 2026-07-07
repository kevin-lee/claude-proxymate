package claudeproxymate.core

/* munit port of the shared hedgehog QueryParamMaskSpec for Scala
 * Native (the hedgehog runner hangs on Native). The property is
 * ported as deterministic representative examples asserting the
 * same invariant. */
class QueryParamMaskSpec extends munit.FunSuite {

  /* ── isSensitive ── */

  test("isSensitive: token") {
    assert(QueryParamMask.isSensitive("token"))
  }

  test("isSensitive: code (extras)") {
    assert(QueryParamMask.isSensitive("code"))
  }

  test("isSensitive: state (extras)") {
    assert(QueryParamMask.isSensitive("state"))
  }

  test("isSensitive: signature (extras)") {
    assert(QueryParamMask.isSensitive("signature"))
  }

  test("isSensitive: api_key (delegates to SensitiveKeys)") {
    assert(QueryParamMask.isSensitive("api_key"))
  }

  test("isSensitive: STATE (case-insensitive)") {
    assert(QueryParamMask.isSensitive("STATE"))
  }

  test("isSensitive: model is NOT sensitive") {
    assert(!QueryParamMask.isSensitive("model"))
  }

  test("isSensitive: empty / null") {
    assert(!QueryParamMask.isSensitive(""))
  }

  /* ── maskPath ── */

  test("no query string returns path unchanged") {
    assertEquals(QueryParamMask.maskPath("/v1/messages"), "/v1/messages")
  }

  test("single sensitive param masks value") {
    assertEquals(QueryParamMask.maskPath("/v1/x?token=abc"), "/v1/x?token=***")
  }

  test("non-sensitive param unchanged") {
    assertEquals(QueryParamMask.maskPath("/v1/x?model=claude"), "/v1/x?model=claude")
  }

  test("two params, first sensitive") {
    assertEquals(QueryParamMask.maskPath("/v1/x?token=abc&model=claude"), "/v1/x?token=***&model=claude")
  }

  test("two params, second sensitive") {
    assertEquals(QueryParamMask.maskPath("/v1/x?model=claude&token=abc"), "/v1/x?model=claude&token=***")
  }

  test("api_key delegates to SensitiveKeys") {
    assertEquals(QueryParamMask.maskPath("/v1/x?api_key=sk-ant-fake"), "/v1/x?api_key=***")
  }

  test("uppercase param name is masked (case-insensitive)") {
    assertEquals(QueryParamMask.maskPath("/v1/x?CODE=foo"), "/v1/x?CODE=***")
  }

  test("multiple `=` in value: only first is the separator") {
    assertEquals(QueryParamMask.maskPath("/v1/x?token=eyJh.eyJh=trail"), "/v1/x?token=***")
  }

  test("empty sensitive value: `key=` -> `key=***`") {
    assertEquals(QueryParamMask.maskPath("/v1/x?token="), "/v1/x?token=***")
  }

  test("param with no `=` (flag-style) emitted unchanged") {
    assertEquals(QueryParamMask.maskPath("/v1/x?keyonly"), "/v1/x?keyonly")
  }

  test("URL fragment preserved after masked query") {
    assertEquals(QueryParamMask.maskPath("/v1/x?token=abc#frag"), "/v1/x?token=***#frag")
  }

  test("user_id delegates to SensitiveKeys (identifier)") {
    assertEquals(QueryParamMask.maskPath("/v1/x?user_id=u-7"), "/v1/x?user_id=***")
  }

  test("repeated non-sensitive key unchanged") {
    assertEquals(QueryParamMask.maskPath("/v1/x?id=1&id=2"), "/v1/x?id=1&id=2")
  }

  test("empty path returns empty") {
    assertEquals(QueryParamMask.maskPath(""), "")
  }

  test("only `?` (empty query) returns unchanged") {
    assertEquals(QueryParamMask.maskPath("/v1/x?"), "/v1/x?")
  }

  /* ── property ported as deterministic examples ── */

  /* Same invariant as the hedgehog property: for a single
   * `name=value` query, the value is masked iff isSensitive(name). */
  private def checkMaskedIffSensitive(name: String, value: String): Unit = {
    val path     = s"/x?$name=$value"
    val out      = QueryParamMask.maskPath(path)
    val expected =
      if (QueryParamMask.isSensitive(name)) s"/x?$name=***"
      else s"/x?$name=$value"
    assertEquals(out, expected, s"input=$path")
  }

  test("random `name=value` is masked iff isSensitive(name) (example: nonce=abc123, extras-sensitive)") {
    checkMaskedIffSensitive("nonce", "abc123")
  }

  test("random `name=value` is masked iff isSensitive(name) (example: csrf=zz9, SensitiveKeys-sensitive)") {
    checkMaskedIffSensitive("csrf", "zz9")
  }

  test("random `name=value` is masked iff isSensitive(name) (example: limit=10, non-sensitive)") {
    checkMaskedIffSensitive("limit", "10")
  }
}
