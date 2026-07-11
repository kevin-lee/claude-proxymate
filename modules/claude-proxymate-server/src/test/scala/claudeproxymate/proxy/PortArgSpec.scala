package claudeproxymate.proxy

class PortArgSpec extends munit.FunSuite {

  test("--port 9090 is parsed") {
    assertEquals(PortArg.parse(List("--port", "9090")), 9090)
  }

  test("--port after other args is parsed") {
    assertEquals(PortArg.parse(List("--verbose", "--port", "9091")), 9091)
  }

  test("missing --port falls back to the default") {
    assertEquals(PortArg.parse(Nil), PortArg.DefaultPort)
    assertEquals(PortArg.parse(List("--verbose")), PortArg.DefaultPort)
  }

  test("--port without a value falls back to the default") {
    assertEquals(PortArg.parse(List("--port")), PortArg.DefaultPort)
  }

  test("non-numeric port falls back to the default") {
    assertEquals(PortArg.parse(List("--port", "abc")), PortArg.DefaultPort)
  }

  test("out-of-range ports fall back to the default") {
    assertEquals(PortArg.parse(List("--port", "80")), PortArg.DefaultPort)
    assertEquals(PortArg.parse(List("--port", "70000")), PortArg.DefaultPort)
  }

  test("range boundaries 1024 and 65535 are accepted") {
    assertEquals(PortArg.parse(List("--port", "1024")), 1024)
    assertEquals(PortArg.parse(List("--port", "65535")), 65535)
  }
}
