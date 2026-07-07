package claudeproxymate.proxy

import scala.scalanative.unsafe._

/** Minimal C bindings for libcurl's easy (synchronous) API. */
@link("curl")
@extern
private[proxy] object LibCurl {
  type CURL     = Ptr[Byte]
  type CURLcode = CInt
  type SList    = Ptr[Byte]

  def curl_global_init(flags: CLong): CURLcode                         = extern
  def curl_easy_init(): CURL                                           = extern
  def curl_easy_cleanup(curl: CURL): Unit                              = extern
  def curl_easy_perform(curl: CURL): CURLcode                          = extern
  def curl_easy_setopt(curl: CURL, option: CInt, args: Any*): CURLcode = extern
  def curl_easy_getinfo(curl: CURL, info: CInt, args: Any*): CURLcode  = extern
  def curl_slist_append(list: SList, str: CString): SList              = extern
  def curl_slist_free_all(list: SList): Unit                           = extern
}

private[proxy] object CurlOpt {
  // CURLOPTTYPE_LONG = 0, OBJECTPOINT = 10000, OFF_T = 30000
  val Url: CInt           = 10002 // CURLOPT_URL
  val WriteData: CInt     = 10001 // CURLOPT_WRITEDATA
  val Header: CInt        = 42 // CURLOPT_HEADER (include headers in output)
  val CustomRequest: CInt = 10036 // CURLOPT_CUSTOMREQUEST
  val HttpHeader: CInt    = 10023 // CURLOPT_HTTPHEADER
  val PostFields: CInt    = 10015 // CURLOPT_POSTFIELDS
  val PostFieldSize: CInt = 60 // CURLOPT_POSTFIELDSIZE
  val Timeout: CInt       = 13 // CURLOPT_TIMEOUT (seconds)
}

private[proxy] object CurlInfo {
  val ResponseCode: CInt = 0x200002 // CURLINFO_RESPONSE_CODE
}
