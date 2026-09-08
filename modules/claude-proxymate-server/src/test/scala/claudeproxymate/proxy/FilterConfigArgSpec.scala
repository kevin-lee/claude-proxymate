package claudeproxymate.proxy

class FilterConfigArgSpec extends munit.FunSuite {

  test("--filter-config <path> is parsed") {
    assertEquals(
      FilterConfigArg.parse(List("--filter-config", "/tmp/request-filter.json")),
      Some("/tmp/request-filter.json")
    )
  }

  test("--filter-config after other args is parsed") {
    assertEquals(
      FilterConfigArg.parse(List("--port", "9090", "--filter-config", "/x/f.json")),
      Some("/x/f.json"),
    )
  }

  test("missing --filter-config yields None") {
    assertEquals(FilterConfigArg.parse(Nil), None)
    assertEquals(FilterConfigArg.parse(List("--port", "9090")), None)
  }

  test("--filter-config without a value yields None") {
    assertEquals(FilterConfigArg.parse(List("--filter-config")), None)
  }
}
