package claudeproxymate.core

import hedgehog.*
import hedgehog.runner.*
import java.nio.file.Path

object I18nPropertiesLoaderSpec extends Properties {

  private val i18nDir: Path = {
    val dir = System.getProperty("i18n.dir")
    if (dir == null) throw new IllegalStateException("System property 'i18n.dir' not set")
    Path.of(dir)
  }

  private lazy val ko: Map[String, String] = I18nPropertiesLoader.load(i18nDir.resolve("ko.properties"))
  private lazy val en: Map[String, String] = I18nPropertiesLoader.load(i18nDir.resolve("en.properties"))

  override def tests: List[Test] = List(
    example("ko and en have the same keys", testSameKeys),
    example("no empty values in ko locale", testNoEmptyKo),
    example("no empty values in en locale", testNoEmptyEn),
    example("variable placeholders match between locales", testVariablePlaceholders),
    example("ko locale loads non-empty map", testKoNonEmpty),
    example("en locale loads non-empty map", testEnNonEmpty),
  )

  def testSameKeys: Result = {
    val koKeys      = ko.keySet
    val enKeys      = en.keySet
    val missingInEn = koKeys -- enKeys
    val missingInKo = enKeys -- koKeys
    Result.all(
      List(
        (missingInEn.size ==== 0).log(s"Keys in ko but not en: $missingInEn"),
        (missingInKo.size ==== 0).log(s"Keys in en but not ko: $missingInKo"),
      )
    )
  }

  def testNoEmptyKo: Result = {
    val empty = ko.filter(_._2.isEmpty)
    (empty.size ==== 0).log(s"Empty ko values: ${empty.keys.mkString(", ")}")
  }

  def testNoEmptyEn: Result = {
    val empty = en.filter(_._2.isEmpty)
    (empty.size ==== 0).log(s"Empty en values: ${empty.keys.mkString(", ")}")
  }

  def testVariablePlaceholders: Result = {
    val varPattern = """\{(\w+)\}""".r
    val issues     = for {
      (key, koVal) <- ko.toList
      enVal        <- en.get(key).toList
      koVars = varPattern.findAllMatchIn(koVal).map(_.group(1)).toSet
      enVars = varPattern.findAllMatchIn(enVal).map(_.group(1)).toSet
      if koVars != enVars
    } yield s"$key: ko=$koVars, en=$enVars"

    (issues.size ==== 0).log(s"Variable placeholder mismatches:\n${issues.mkString("\n")}")
  }

  def testKoNonEmpty: Result =
    Result.assert(ko.nonEmpty).log("ko locale should have entries")

  def testEnNonEmpty: Result =
    Result.assert(en.nonEmpty).log("en locale should have entries")
}
