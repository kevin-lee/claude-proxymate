package claudeproxymate.proxy

class ParentLinkArgSpec extends munit.FunSuite {

  test("--exit-on-stdin-close alone is parsed") {
    assertEquals(ParentLinkArg.parse(List(ParentLinkArg.Flag)), ParentLink.WatchStdin)
  }

  test("--exit-on-stdin-close after other args is parsed") {
    assertEquals(
      ParentLinkArg.parse(List("--port", "9090", "--filter-config", "/tmp/f.json", ParentLinkArg.Flag)),
      ParentLink.WatchStdin,
    )
  }

  test("--exit-on-stdin-close before other args is parsed") {
    assertEquals(ParentLinkArg.parse(List(ParentLinkArg.Flag, "--port", "9090")), ParentLink.WatchStdin)
  }

  test("missing --exit-on-stdin-close falls back to Detached") {
    assertEquals(ParentLinkArg.parse(List("--port", "9090")), ParentLink.Detached)
  }

  test("empty args fall back to Detached") {
    assertEquals(ParentLinkArg.parse(Nil), ParentLink.Detached)
  }

  test("the flag name is exactly --exit-on-stdin-close") {
    assertEquals(ParentLinkArg.Flag, "--exit-on-stdin-close")
  }
}
