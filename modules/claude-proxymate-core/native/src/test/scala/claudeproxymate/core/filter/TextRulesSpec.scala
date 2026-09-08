package claudeproxymate.core.filter

/* munit port of the shared hedgehog TextRulesSpec for Scala Native, where the
 * hedgehog runner hangs. Examples port 1:1; the literal-length property is
 * replaced by examples at the generator's bounds. The RE2 boundary case
 * (lookahead) is Native-only: the JVM regex accepts it.
 */
class TextRulesSpec extends munit.FunSuite {

  import FilterFixtures.rule

  private def compiled(r: TextRule): CompiledRule =
    TextRules.compile(r).fold(err => fail(err), identity)

  test("Text removes every occurrence and counts bytes") {
    val (out, hits) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Text, "a", TextRuleScope.Both))), "a-b-a-b")
    assertEquals(out, "-b--b")
    assertEquals(hits.map(_.bytes), List(2))
  }

  test("Tag removes the tag and its content, keeps siblings") {
    val (out, hits) =
      TextRules.applyAll(
        List(compiled(rule(TextRuleKind.Tag, "t", TextRuleScope.Both))),
        "<t>x</t>y<t>z</t> <u>keep</u>"
      )
    assertEquals(out, "y <u>keep</u>")
    assertEquals(hits.map(_.bytes), List(16))
  }

  test("Tag with attributes and newlines inside is removed") {
    val text     = "before <my-tag id=\"1\">line1\nline2</my-tag> after"
    val (out, _) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both))), text)
    assertEquals(out, "before  after")
  }

  test("invalid tag name fails to compile") {
    assert(TextRules.compile(rule(TextRuleKind.Tag, "<bad>", TextRuleScope.Both)).isLeft)
  }

  test("regex syntax error fails to compile") {
    assert(TextRules.compile(rule(TextRuleKind.Regex, "(", TextRuleScope.Both)).isLeft)
  }

  test("lookahead is rejected by the RE2 engine and reported, not thrown (Native only)") {
    val result = TextRules.compile(rule(TextRuleKind.Regex, "secret(?= plan)", TextRuleScope.Both))
    assert(result.isLeft, s"lookahead should fail to compile on RE2, got $result")
    assert(result.left.exists(_.startsWith("regex secret(?= plan): ")), s"reason should name the rule: $result")
  }

  test("empty patterns fail to compile") {
    assert(TextRules.compile(rule(TextRuleKind.Text, "", TextRuleScope.Both)).isLeft)
    assert(TextRules.compile(rule(TextRuleKind.Regex, "", TextRuleScope.Both)).isLeft)
    assert(TextRules.compile(rule(TextRuleKind.Tag, "", TextRuleScope.Both)).isLeft)
  }

  test("Regex removes matches") {
    val (out, hits) =
      TextRules.applyAll(List(compiled(rule(TextRuleKind.Regex, "[0-9]+", TextRuleScope.Both))), "a1b22c333")
    assertEquals(out, "abc")
    assertEquals(hits.map(_.bytes), List(6))
  }

  test("Regex inline (?s) and (?m) flags work on RE2") {
    val dotAll    = compiled(rule(TextRuleKind.Regex, "(?s)<x>.*</x>", TextRuleScope.Both))
    val multiline = compiled(rule(TextRuleKind.Regex, "(?m)^# line$", TextRuleScope.Both))
    assertEquals(TextRules.applyAll(List(dotAll), "a<x>1\n2</x>b")._1, "ab")
    assertEquals(TextRules.applyAll(List(multiline), "# line\nkeep\n# line")._1, "\nkeep\n")
  }

  test("rules apply in order, each on the previous output") {
    val rules    = List(
      compiled(rule(TextRuleKind.Text, "b", TextRuleScope.Both)),
      compiled(rule(TextRuleKind.Text, "ac", TextRuleScope.Both)),
    )
    val (out, _) = TextRules.applyAll(rules, "abc")
    assertEquals(out, "")
  }

  test("inScope") {
    assert(TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Both), TextRuleScope.UserTyped))
    assert(TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Both), TextRuleScope.Assistant))
    assert(!TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.UserTyped), TextRuleScope.Assistant))
    assert(!TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Assistant), TextRuleScope.UserTyped))
  }

  private def assertLiteralLength(literal: String, text: String): Unit = {
    val (out, hits) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Text, literal, TextRuleScope.Both))), text)
    val removed     = hits.map(_.bytes).sum
    assertEquals(out.length, text.length - removed)
    assertEquals(removed % literal.length, 0)
  }

  test("literal removal shortens the text by count times the literal length (example: 1-char literal)") {
    assertLiteralLength("a", "abacaba")
  }

  test("literal removal shortens the text by count times the literal length (example: 3-char literal)") {
    assertLiteralLength("aba", "ababababa")
  }

  test("literal removal shortens the text by count times the literal length (example: no occurrence)") {
    assertLiteralLength("bb", "acacac")
  }
}
