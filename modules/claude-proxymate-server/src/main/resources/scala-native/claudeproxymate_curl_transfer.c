/*
 * C helpers for the streaming libcurl client (CurlHttpClient.scala, issue #100).
 *
 * Why C: curl_easy_perform runs with @blocking, so the thread is "Unmanaged"
 * for the GC while libcurl works. A libcurl callback written in Scala would run
 * in that state, the GC would not scan its frames, and the heap could be
 * corrupted. So every libcurl callback of the proxy is C code:
 *   - the write path is libcurl's default write function, the C fwrite into
 *     the pipe that CURLOPT_WRITEDATA names (no callback is set), and
 *   - the transfer-info callback below only reads an int flag.
 *
 * The prototypes of libcurl are declared here, not included from
 * <curl/curl.h>, so the build does not need the libcurl headers. The option
 * numbers are the ones in curl.h (CURLOPTTYPE_* + n). They are part of the
 * libcurl ABI and do not change.
 */
#include <signal.h>
#include <stddef.h>

typedef void CURL;
typedef int CURLcode;
extern CURLcode curl_easy_setopt(CURL *curl, int option, ...);

#define CLAUDEPROXYMATE_CURLOPT_NOPROGRESS 43           /* CURLOPTTYPE_LONG + 43 */
#define CLAUDEPROXYMATE_CURLOPT_XFERINFODATA 10057      /* CURLOPTTYPE_CBPOINT + 57 */
#define CLAUDEPROXYMATE_CURLOPT_XFERINFOFUNCTION 20219  /* CURLOPTTYPE_FUNCTIONPOINT + 219 */

/*
 * CURLOPT_XFERINFOFUNCTION. libcurl calls it often during the transfer, and
 * about once each second also when no data comes. A non-zero return value makes
 * libcurl stop the transfer with CURLE_ABORTED_BY_CALLBACK (42).
 * curl_off_t is a 64-bit signed integer on the supported platforms.
 */
static int claudeproxymate_curl_xferinfo(void *flag, long long dltotal, long long dlnow, long long ultotal,
                                         long long ulnow) {
    (void)dltotal;
    (void)dlnow;
    (void)ultotal;
    (void)ulnow;
    return __atomic_load_n((const int *)flag, __ATOMIC_ACQUIRE) != 0;
}

/*
 * Make libcurl stop the transfer of `curl` when the int at `flag` is not 0.
 * `flag` must stay valid until curl_easy_perform has returned.
 * Returns the first CURLcode that is not CURLE_OK (0), else 0.
 */
int claudeproxymate_curl_use_abort_flag(CURL *curl, int *flag) {
    CURLcode rc = curl_easy_setopt(curl, CLAUDEPROXYMATE_CURLOPT_XFERINFOFUNCTION, claudeproxymate_curl_xferinfo);
    if (rc != 0) {
        return rc;
    }
    rc = curl_easy_setopt(curl, CLAUDEPROXYMATE_CURLOPT_XFERINFODATA, (void *)flag);
    if (rc != 0) {
        return rc;
    }
    return curl_easy_setopt(curl, CLAUDEPROXYMATE_CURLOPT_NOPROGRESS, 0L);
}

/* Set the flag. Safe to call from any thread, also while libcurl reads it. */
void claudeproxymate_curl_abort(int *flag) {
    __atomic_store_n(flag, 1, __ATOMIC_RELEASE);
}

/*
 * Ignore SIGPIPE in the whole process. When Claude Code disconnects, the proxy
 * closes the read end of a pipe, and libcurl's next fwrite into the write end
 * then fails with EPIPE. Without this, that write would also send SIGPIPE, and
 * the default action of SIGPIPE stops the process.
 * Returns 0, or -1 when signal() fails.
 */
int claudeproxymate_ignore_sigpipe(void) {
    return signal(SIGPIPE, SIG_IGN) == SIG_ERR ? -1 : 0;
}
