package claudeproxymate.renderer.detail

/** Logic for detecting and handling external-link clicks inside the token
  * popover. Extracted from [[TokenPopover]] so it can be unit-tested without
  * bootstrapping a full DOM event loop.
  */
object TokenPopoverLinks {

  /** CSS class marking an anchor whose clicks should be routed through
    * `electronAPI.openExternal` instead of letting the browser navigate.
    */
  val ExternalLinkClass: String = "token-popover-link"

  /** If `classNames` contains [[ExternalLinkClass]] and `href` is non-empty,
    * return `Some(href)` — caller should invoke `electronAPI.openExternal(href)`.
    * Otherwise return `None` and the caller should fall through to other
    * click-handling branches.
    */
  def extractExternalLinkHref(classNames: List[String], href: String): Option[String] = {
    if (classNames.contains(ExternalLinkClass) && href.nonEmpty) Some(href)
    else None
  }
}
