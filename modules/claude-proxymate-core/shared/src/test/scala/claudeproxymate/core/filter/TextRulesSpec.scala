package claudeproxymate.core.filter

import hedgehog.*
import hedgehog.runner.*

import FilterFixtures.rule

object TextRulesSpec extends Properties {

  override def tests: List[Test] = List(
    example("Text removes every occurrence and counts bytes", testLiteral),
    example("Tag removes the tag and its content, keeps siblings", testTag),
    example("Tag with attributes and newlines inside is removed", testTagAttributes),
    example("invalid tag name fails to compile", testInvalidTag),
    example("regex syntax error fails to compile", testInvalidRegex),
    example("empty patterns fail to compile", testEmptyPattern),
    example("Regex removes matches", testRegex),
    example("rules apply in order, each on the previous output", testOrder),
    example("inScope", testInScope),
    property("literal removal shortens the text by count times the literal length", testLiteralProperty),
  )

  private def compiled(r: TextRule): CompiledRule =
    TextRules.compile(r).fold(err => throw new AssertionError(err), identity)

  def testLiteral: Result = {
    val (out, hits) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Text, "a", TextRuleScope.Both))), "a-b-a-b")
    Result.all(
      List(
        (out ==== "-b--b").log("remaining text"),
        (hits.map(_.bytes) ==== List(2)).log("bytes removed"),
      )
    )
  }

  def testTag: Result = {
    val text        = "<t>x</t>y<t>z</t> <u>keep</u>"
    val (out, hits) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Tag, "t", TextRuleScope.Both))), text)
    Result.all(
      List(
        (out ==== "y <u>keep</u>").log("remaining text"),
        (hits.map(_.bytes) ==== List(16)).log("bytes removed"),
      )
    )
  }

  def testTagAttributes: Result = {
    val text     = "before <my-tag id=\"1\">line1\nline2</my-tag> after"
    val (out, _) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Tag, "my-tag", TextRuleScope.Both))), text)
    (out ==== "before  after").log("tag with attributes and newline should be removed")
  }

  def testInvalidTag: Result =
    Result.assert(TextRules.compile(rule(TextRuleKind.Tag, "<bad>", TextRuleScope.Both)).isLeft).log("invalid tag name")

  def testInvalidRegex: Result =
    Result.assert(TextRules.compile(rule(TextRuleKind.Regex, "(", TextRuleScope.Both)).isLeft).log("unbalanced paren")

  def testEmptyPattern: Result =
    Result.all(
      List(
        Result.assert(TextRules.compile(rule(TextRuleKind.Text, "", TextRuleScope.Both)).isLeft).log("empty text"),
        Result.assert(TextRules.compile(rule(TextRuleKind.Regex, "", TextRuleScope.Both)).isLeft).log("empty regex"),
        Result.assert(TextRules.compile(rule(TextRuleKind.Tag, "", TextRuleScope.Both)).isLeft).log("empty tag"),
      )
    )

  def testRegex: Result = {
    val (out, hits) =
      TextRules.applyAll(List(compiled(rule(TextRuleKind.Regex, "[0-9]+", TextRuleScope.Both))), "a1b22c333")
    Result.all(
      List(
        (out ==== "abc").log("digits removed"),
        (hits.map(_.bytes) ==== List(6)).log("bytes removed"),
      )
    )
  }

  def testOrder: Result = {
    val rules    = List(
      compiled(rule(TextRuleKind.Text, "b", TextRuleScope.Both)),
      compiled(rule(TextRuleKind.Text, "ac", TextRuleScope.Both)),
    )
    val (out, _) = TextRules.applyAll(rules, "abc")
    (out ==== "").log("removing b first exposes ac to the second rule")
  }

  def testInScope: Result =
    Result.all(
      List(
        Result
          .assert(TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Both), TextRuleScope.UserTyped))
          .log("both/user"),
        Result
          .assert(TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Both), TextRuleScope.Assistant))
          .log("both/asst"),
        Result
          .assert(!TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.UserTyped), TextRuleScope.Assistant))
          .log("user/asst"),
        Result
          .assert(!TextRules.inScope(rule(TextRuleKind.Text, "x", TextRuleScope.Assistant), TextRuleScope.UserTyped))
          .log("asst/user"),
      )
    )

  def testLiteralProperty: Property =
    for {
      literal <- Gen.string(Gen.element1('a', 'b'), Range.linear(1, 3)).log("literal")
      text    <- Gen.string(Gen.element1('a', 'b', 'c'), Range.linear(0, 30)).log("text")
    } yield {
      val (out, hits) = TextRules.applyAll(List(compiled(rule(TextRuleKind.Text, literal, TextRuleScope.Both))), text)
      val removed     = hits.map(_.bytes).sum
      Result.all(
        List(
          (out.length ==== (text.length - removed)).log(s"length mismatch: out=$out removed=$removed"),
          Result.assert(removed % literal.length == 0).log("removed bytes must be a multiple of the literal length"),
        )
      )
    }
}
