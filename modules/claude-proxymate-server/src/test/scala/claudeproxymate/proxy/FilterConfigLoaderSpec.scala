package claudeproxymate.proxy

import claudeproxymate.core.filter.{CategoryMode, FilterConfig}

/* Only the pure `parse` is unit-tested here; the file-reading path of
 * `FilterConfigLoader.load` is exercised manually with the Electron app. */
class FilterConfigLoaderSpec extends munit.FunSuite {

  test("valid config text parses") {
    val text = """{"version":1,"enabled":true,"rules":{"mode":"removeAll","keys":[]},"textRules":[]}"""
    assertEquals(
      FilterConfigLoader.parse(text).map(c => (c.enabled, c.rules.mode)),
      Right((true, CategoryMode.RemoveAll)),
    )
  }

  test("an empty object parses to the default config") {
    assertEquals(FilterConfigLoader.parse("{}"), Right(FilterConfig.default))
  }

  test("invalid JSON fails") {
    assert(FilterConfigLoader.parse("{not json").isLeft)
  }

  test("wrong shape fails") {
    assert(FilterConfigLoader.parse("""{"rules":{"mode":"bogus"}}""").isLeft)
    assert(FilterConfigLoader.parse("[]").isLeft)
  }
}
