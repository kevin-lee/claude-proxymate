package claudeproxymate.core.filter

import cats.syntax.all.*

import java.util.regex.PatternSyntaxException
import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

/** A text rule ready to run: `matches` returns the half-open spans to cut. */
final case class CompiledRule(rule: TextRule, matches: String => List[(Int, Int)])

/** Bytes (UTF-8) one rule removed from one text. */
final case class TextHit(rule: TextRule, bytes: Int)

/** Compiles and applies Text / Regex / Tag rules.
  *
  * Regex rules run on `java.util.regex`, which is RE2-based on Scala Native:
  * lookaround and backreferences fail at compile time and the rule is
  * reported as skipped instead of crashing the proxy.
  */
object TextRules {

  val TagNamePattern: Regex = """^[A-Za-z][A-Za-z0-9_.:-]*$""".r

  def inScope(rule: TextRule, scope: TextRuleScope): Boolean =
    rule.scope === TextRuleScope.Both || rule.scope === scope

  def compile(rule: TextRule): Either[String, CompiledRule] = rule.kind match {
    case TextRuleKind.Text =>
      if (rule.pattern.isEmpty) Left("text: empty pattern")
      else Right(CompiledRule(rule, literalMatches(rule.pattern, _)))

    case TextRuleKind.Regex =>
      if (rule.pattern.isEmpty) Left("regex: empty pattern")
      else {
        Try(rule.pattern.r)
          .toEither
          .left
          .map {
            case e: PatternSyntaxException => s"regex ${rule.pattern}: ${e.getDescription}"
            case e => s"regex ${rule.pattern}: ${Option(e.getMessage).getOrElse(e.getClass.getName)}"
          }
          .map(re => CompiledRule(rule, regexMatches(re, _)))
      }

    case TextRuleKind.Tag =>
      if (!TagNamePattern.matches(rule.pattern)) Left(s"tag ${rule.pattern}: invalid tag name")
      else {
        val q  = Regex.quote(rule.pattern)
        val re = s"(?s)<$q(?:\\s[^>]*)?>.*?</$q>".r
        Right(CompiledRule(rule, regexMatches(re, _)))
      }
  }

  /** Applies the rules in order, each on the output of the previous one, and
    * returns the text with every match cut out plus the bytes each rule removed
    * (rules that removed nothing are not listed).
    */
  def applyAll(rules: List[CompiledRule], text: String): (String, List[TextHit]) =
    rules.foldLeft((text, List.empty[TextHit])) {
      case ((current, hits), compiled) =>
        val spans = compiled.matches(current).filter { case (s, e) => e > s }
        if (spans.isEmpty) (current, hits)
        else {
          val removedBytes = spans.map { case (s, e) => byteLen(current.substring(s, e)) }.sum
          val (kept, last) = spans.foldLeft((new StringBuilder, 0)) {
            case ((sb, pos), (s, e)) =>
              (sb.append(current.substring(pos, s)), e)
          }
          (kept.append(current.substring(last)).toString, hits :+ TextHit(compiled.rule, removedBytes))
        }
    }

  def byteLen(s: String): Int = s.getBytes("UTF-8").length

  private def literalMatches(literal: String, text: String): List[(Int, Int)] = {
    @tailrec
    def loop(from: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val idx = text.indexOf(literal, from)
      if (idx < 0) acc.reverse else loop(idx + literal.length, (idx, idx + literal.length) :: acc)
    }
    loop(0, Nil)
  }

  private def regexMatches(re: Regex, text: String): List[(Int, Int)] =
    re.findAllMatchIn(text).map(m => (m.start, m.end)).toList
}
