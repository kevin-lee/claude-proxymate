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
  /* curl_easy_perform blocks the calling thread for the whole HTTP round trip.
   * @blocking makes the call notify the GC that the thread is in unmanaged
   * code, so a collection triggered by another request does not wait for it.
   * Safe only because nothing in the call re-enters Scala: the response goes
   * through libcurl's default write function (the C fwrite into the pipe that
   * CURLOPT_WRITEDATA names), and the only callback is the C function in
   * claudeproxymate_curl_transfer.c. Never set a Scala CFuncPtr callback. */
  @blocking
  def curl_easy_perform(curl: CURL): CURLcode                          = extern
  def curl_easy_setopt(curl: CURL, option: CInt, args: Any*): CURLcode = extern
  def curl_slist_append(list: SList, str: CString): SList              = extern
  def curl_slist_free_all(list: SList): Unit                           = extern
}

private[proxy] object CurlOpt {
  // CURLOPTTYPE_LONG = 0, OBJECTPOINT = 10000, FUNCTIONPOINT = 20000, OFF_T = 30000
  val Url: CInt            = 10002 // CURLOPT_URL
  val WriteData: CInt      = 10001 // CURLOPT_WRITEDATA
  val Header: CInt         = 42 // CURLOPT_HEADER (include headers in output)
  val CustomRequest: CInt  = 10036 // CURLOPT_CUSTOMREQUEST
  val HttpHeader: CInt     = 10023 // CURLOPT_HTTPHEADER
  val PostFieldSize: CInt  = 60 // CURLOPT_POSTFIELDSIZE
  val CopyPostFields: CInt = 10165 // CURLOPT_COPYPOSTFIELDS
  val LowSpeedLimit: CInt  = 19 // CURLOPT_LOW_SPEED_LIMIT (bytes per second)
  val LowSpeedTime: CInt   = 20 // CURLOPT_LOW_SPEED_TIME (seconds)
  val ConnectTimeout: CInt = 78 // CURLOPT_CONNECTTIMEOUT (seconds)
  val NoSignal: CInt       = 99 // CURLOPT_NOSIGNAL
}

private[proxy] object CurlCode {
  val Ok: CInt                = 0 // CURLE_OK
  val WriteError: CInt        = 23 // CURLE_WRITE_ERROR
  val AbortedByCallback: CInt = 42 // CURLE_ABORTED_BY_CALLBACK
}

private[proxy] object CurlGlobal {
  val All: CLong = 3.toSize // CURL_GLOBAL_ALL = CURL_GLOBAL_SSL | CURL_GLOBAL_WIN32
}

/** Bindings for the C helpers in
  * `src/main/resources/scala-native/claudeproxymate_curl_transfer.c`.
  */
@extern
private[proxy] object CurlTransferNative {

  /** Make libcurl stop the transfer when the int at `flag` is not 0. */
  def claudeproxymate_curl_use_abort_flag(curl: LibCurl.CURL, flag: Ptr[CInt]): CInt = extern

  /** Set the flag (atomic store). */
  def claudeproxymate_curl_abort(flag: Ptr[CInt]): Unit = extern

  /** Ignore SIGPIPE in the process. Returns 0, or -1 on failure. */
  def claudeproxymate_ignore_sigpipe(): CInt = extern
}
