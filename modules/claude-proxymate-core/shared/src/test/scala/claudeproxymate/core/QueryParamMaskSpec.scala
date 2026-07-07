package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*

object QueryParamMaskSpec extends Properties {

  override def tests: List[Test] = List(
    // isSensitive
    example("isSensitive: token", testIsSensitiveToken),
    example("isSensitive: code (extras)", testIsSensitiveCode),
    example("isSensitive: state (extras)", testIsSensitiveState),
    example("isSensitive: signature (extras)", testIsSensitiveSignature),
    example("isSensitive: api_key (delegates to SensitiveKeys)", testIsSensitiveApiKey),
    example("isSensitive: STATE (case-insensitive)", testIsSensitiveUppercase),
    example("isSensitive: model is NOT sensitive", testIsSensitiveModel),
    example("isSensitive: empty / null", testIsSensitiveEmpty),
    // maskPath
    example("no query string returns path unchanged", testNoQuery),
    example("single sensitive param masks value", testSingleSensitive),
    example("non-sensitive param unchanged", testNonSensitive),
    example("two params, first sensitive", testTwoFirstSensitive),
    example("two params, second sensitive", testTwoSecondSensitive),
    example("api_key delegates to SensitiveKeys", testApiKey),
    example("uppercase param name is masked (case-insensitive)", testUppercaseParam),
    example("multiple `=` in value: only first is the separator", testMultipleEquals),
    example("empty sensitive value: `key=` -> `key=***`", testEmptyValueSensitive),
    example("param with no `=` (flag-style) emitted unchanged", testFlagStyleParam),
    example("URL fragment preserved after masked query", testFragmentPreserved),
    example("user_id delegates to SensitiveKeys (identifier)", testUserId),
    example("repeated non-sensitive key unchanged", testRepeatedKeyNonSensitive),
    example("empty path returns empty", testEmptyPath),
    example("only `?` (empty query) returns unchanged", testOnlyQuestion),
    // property
    property("random `name=value` is masked iff isSensitive(name)", testRandomMaskingPredicate),
  )

  private def must(s: String, expected: Boolean): Result =
    Result
      .assert(QueryParamMask.isSensitive(s) == expected)
      .log(s"isSensitive(\"$s\") expected $expected")

  // ── isSensitive ────────────────────────────────────────────────────────

  def testIsSensitiveToken: Result     = must("token", true)
  def testIsSensitiveCode: Result      = must("code", true)
  def testIsSensitiveState: Result     = must("state", true)
  def testIsSensitiveSignature: Result = must("signature", true)
  def testIsSensitiveApiKey: Result    = must("api_key", true)
  def testIsSensitiveUppercase: Result = must("STATE", true)
  def testIsSensitiveModel: Result     = must("model", false)
  def testIsSensitiveEmpty: Result     = must("", false)

  // ── maskPath ───────────────────────────────────────────────────────────

  private def equal(actual: String, expected: String): Result =
    Result
      .assert(actual == expected)
      .log(s"expected `$expected`, got `$actual`")

  def testNoQuery: Result =
    equal(QueryParamMask.maskPath("/v1/messages"), "/v1/messages")

  def testSingleSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?token=abc"), "/v1/x?token=***")

  def testNonSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?model=claude"), "/v1/x?model=claude")

  def testTwoFirstSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?token=abc&model=claude"), "/v1/x?token=***&model=claude")

  def testTwoSecondSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?model=claude&token=abc"), "/v1/x?model=claude&token=***")

  def testApiKey: Result =
    equal(QueryParamMask.maskPath("/v1/x?api_key=sk-ant-fake"), "/v1/x?api_key=***")

  def testUppercaseParam: Result =
    equal(QueryParamMask.maskPath("/v1/x?CODE=foo"), "/v1/x?CODE=***")

  def testMultipleEquals: Result =
    equal(QueryParamMask.maskPath("/v1/x?token=eyJh.eyJh=trail"), "/v1/x?token=***")

  def testEmptyValueSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?token="), "/v1/x?token=***")

  def testFlagStyleParam: Result =
    equal(QueryParamMask.maskPath("/v1/x?keyonly"), "/v1/x?keyonly")

  def testFragmentPreserved: Result =
    equal(QueryParamMask.maskPath("/v1/x?token=abc#frag"), "/v1/x?token=***#frag")

  def testUserId: Result =
    equal(QueryParamMask.maskPath("/v1/x?user_id=u-7"), "/v1/x?user_id=***")

  def testRepeatedKeyNonSensitive: Result =
    equal(QueryParamMask.maskPath("/v1/x?id=1&id=2"), "/v1/x?id=1&id=2")

  def testEmptyPath: Result =
    equal(QueryParamMask.maskPath(""), "")

  def testOnlyQuestion: Result =
    equal(QueryParamMask.maskPath("/v1/x?"), "/v1/x?")

  // ── property ───────────────────────────────────────────────────────────

  def testRandomMaskingPredicate: Property =
    for {
      name  <- Gen.string(Gen.alpha, Range.linear(1, 12)).log("name")
      value <- Gen.string(Gen.alphaNum, Range.linear(1, 20)).log("value")
    } yield {
      val path     = s"/x?$name=$value"
      val out      = QueryParamMask.maskPath(path)
      val expected =
        if (QueryParamMask.isSensitive(name)) s"/x?$name=***"
        else s"/x?$name=$value"
      Result
        .assert(out == expected)
        .log(s"input=$path got=$out expected=$expected")
    }
}
