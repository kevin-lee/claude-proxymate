package claudeproxymate.proxy

import claudeproxymate.core.ProxyError

/** A [[ProxyError]] raised in `IO`, where a value cannot be returned: during the
  * set-up of a transfer, or in the body stream after the headers were sent.
  */
final case class ProxyErrorException(error: ProxyError) extends RuntimeException(error.message)
