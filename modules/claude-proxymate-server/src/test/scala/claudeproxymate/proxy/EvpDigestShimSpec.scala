package claudeproxymate.proxy

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

/* The same signatures as http4s-crypto's `object evp`. @link("crypto") makes the
 * Linux test binary link the real libcrypto, the same as the production binary. */
@link("crypto")
@extern
private object EvpFfi {
  def EVP_get_digestbyname(name: CString): Ptr[Byte] = extern

  def EVP_Digest(
    data: Ptr[Byte],
    count: CSize,
    md: Ptr[CUnsignedChar],
    size: Ptr[CUnsignedInt],
    `type`: Ptr[Byte],
    impl: Ptr[Byte],
  ): CInt = extern
}

/* On macOS this tests claudeproxymate_evp_digest_darwin.c (the binary has no
 * libcrypto load command). On Linux it tests OpenSSL. The same expected values on
 * both prove that the shim is a correct replacement for the two OpenSSL functions
 * that http4s-crypto uses (issue #95). */
class EvpDigestShimSpec extends munit.FunSuite {

  final private case class DigestResult(result: Int, size: Int, hex: String)

  /* Calls both functions the way http4s-crypto's HashPlatform does. Returns the
   * result code and the digest bytes (as many as the reported size). When the
   * result is not 1, the digest bytes are empty, because *size is not set then. */
  private def runDigest(algorithm: String, input: Array[Byte]): (Int, Array[Byte]) =
    Zone.acquire { implicit z =>
      val digestType = EvpFfi.EVP_get_digestbyname(toCString(algorithm))
      val data       = alloc[Byte](math.max(input.length, 1).toUSize)
      input.indices.foreach(i => data(i) = input(i))
      val md         = alloc[CUnsignedChar](64.toUSize)
      val size       = alloc[CUnsignedInt]()
      !size = 0.toUInt
      val result     = EvpFfi.EVP_Digest(data, input.length.toUSize, md, size, digestType, null)
      (result, if (result == 1) Array.tabulate((!size).toInt)(i => md(i).toByte) else Array.emptyByteArray)
    }

  private def digest(algorithm: String, input: Array[Byte]): DigestResult = {
    val (result, bytes) = runDigest(algorithm, input)
    DigestResult(result, bytes.length, bytes.map(b => f"${b & 0xff}%02x").mkString)
  }

  private def isNull(name: String): Boolean =
    Zone.acquire { implicit z =>
      EvpFfi.EVP_get_digestbyname(toCString(name)) == null
    }

  private val abc: Array[Byte] = "abc".getBytes(StandardCharsets.US_ASCII)

  test("EVP_get_digestbyname knows MD5, SHA1, SHA256 and SHA512") {
    List("MD5", "SHA1", "SHA256", "SHA512").foreach { name =>
      assert(!isNull(name), s"$name gave a null pointer")
    }
  }

  test("EVP_get_digestbyname returns null for an unknown name") {
    assert(isNull("NOPE"))
  }

  test("MD5 of \"abc\"") {
    assertEquals(digest("MD5", abc), DigestResult(1, 16, "900150983cd24fb0d6963f7d28e17f72"))
  }

  test("SHA1 of \"abc\"") {
    assertEquals(digest("SHA1", abc), DigestResult(1, 20, "a9993e364706816aba3e25717850c26c9cd0d89d"))
  }

  test("SHA256 of \"abc\"") {
    assertEquals(
      digest("SHA256", abc),
      DigestResult(1, 32, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"),
    )
  }

  test("SHA512 of \"abc\"") {
    assertEquals(
      digest("SHA512", abc),
      DigestResult(
        1,
        64,
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f",
      ),
    )
  }

  test("SHA1 of the empty input") {
    assertEquals(
      digest("SHA1", Array.emptyByteArray),
      DigestResult(1, 20, "da39a3ee5e6b4b0d3255bfef95601890afd80709"),
    )
  }

  /* RFC 6455 section 1.3. This is the computation of the Ember server's
   * WebSocketHelpers.serverHandshake. */
  test("the WebSocket accept value from RFC 6455") {
    val key            = "dGhlIHNhbXBsZSBub25jZQ=="
    val magic          = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    val (result, sha1) = runDigest("SHA1", (key + magic).getBytes(StandardCharsets.US_ASCII))
    assertEquals(result, 1)
    assertEquals(Base64.getEncoder.encodeToString(sha1), "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
  }

  /* The error paths below are of the macOS shim only. */
  private def assumeMacShim(): Unit =
    assume(
      scala.util.Properties.isMac,
      "the error paths are of the macOS shim. OpenSSL behaves differently and can crash",
    )

  /* Calls EVP_Digest with "abc" and the given choices of null pointers. Returns
   * the result code and the first 20 bytes of md as hex (empty when md is null). */
  private def digestAbcWith(
    nullData: Boolean,
    nullMd: Boolean,
    nullSize: Boolean,
    digestType: Zone ?=> Ptr[Byte],
  ): (Int, String) =
    Zone.acquire { implicit z =>
      val data   = alloc[Byte](3.toUSize)
      abc.indices.foreach(i => data(i) = abc(i))
      val md     = alloc[CUnsignedChar](64.toUSize)
      val size   = alloc[CUnsignedInt]()
      val result = EvpFfi.EVP_Digest(
        if (nullData) null else data,
        3.toUSize,
        if (nullMd) null else md,
        if (nullSize) null else size,
        digestType,
        null,
      )
      val hex    = if (nullMd) "" else (0 until 20).map(i => f"${md(i).toInt}%02x").mkString
      (result, hex)
    }

  private def sha1Type(using Zone): Ptr[Byte] = EvpFfi.EVP_get_digestbyname(toCString("SHA1"))

  test("macOS shim: md is null gives 0") {
    assumeMacShim()
    assertEquals(digestAbcWith(nullData = false, nullMd = true, nullSize = false, sha1Type)._1, 0)
  }

  test("macOS shim: data is null with count 3 gives 0") {
    assumeMacShim()
    assertEquals(digestAbcWith(nullData = true, nullMd = false, nullSize = false, sha1Type)._1, 0)
  }

  test("macOS shim: type is null gives 0") {
    assumeMacShim()
    assertEquals(digestAbcWith(nullData = false, nullMd = false, nullSize = false, null)._1, 0)
  }

  test("macOS shim: a type that is not from EVP_get_digestbyname gives 0") {
    assumeMacShim()
    assertEquals(
      digestAbcWith(nullData = false, nullMd = false, nullSize = false, alloc[Byte](1.toUSize))._1,
      0,
    )
  }

  test("macOS shim: size is null still gives the digest") {
    assumeMacShim()
    assertEquals(
      digestAbcWith(nullData = false, nullMd = false, nullSize = true, sha1Type),
      (1, "a9993e364706816aba3e25717850c26c9cd0d89d"),
    )
  }

  /* On the shim, EVP_get_digestbyname("NOPE") gives null, and EVP_Digest with a
   * null type gives 0. macOS only, because OpenSSL can crash with a null type. */
  test("macOS shim: digest with an unknown name gives 0 and no bytes") {
    assumeMacShim()
    assertEquals(digest("NOPE", abc), DigestResult(0, 0, ""))
  }
}
