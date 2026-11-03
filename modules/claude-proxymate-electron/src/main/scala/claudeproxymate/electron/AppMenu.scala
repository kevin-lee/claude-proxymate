package claudeproxymate.electron

import claudeproxymate.electron.facades.Menu

import scala.scalajs.js

/** The macOS application menu: Electron's default menus plus "Check for Updates…".
  *
  * The app menu is written out so "Check for Updates…" sits right after "About".
  * The other menus are Electron's roles, so copy, paste, zoom, reload and minimize
  * work as they did with the default menu. The default Help menu (Electron links)
  * is left out. The text is English only, the same as Electron's default menu.
  */
object AppMenu {

  private def role(name: String): js.Object = js.Dynamic.literal(role = name).asInstanceOf[js.Object]

  private val separator: js.Object = js.Dynamic.literal(`type` = "separator").asInstanceOf[js.Object]

  def install(onCheckForUpdates: () => Unit): Unit = {
    val checkForUpdates = js
      .Dynamic
      .literal(
        label = "Check for Updates…",
        click = { () => onCheckForUpdates() }: js.Function0[Unit],
      )
      .asInstanceOf[js.Object]

    val appMenu = js
      .Dynamic
      .literal(
        role = "appMenu",
        submenu = js.Array(
          role("about"),
          checkForUpdates,
          separator,
          role("services"),
          separator,
          role("hide"),
          role("hideOthers"),
          role("unhide"),
          separator,
          role("quit"),
        ),
      )
      .asInstanceOf[js.Object]

    val template = js.Array(appMenu, role("fileMenu"), role("editMenu"), role("viewMenu"), role("windowMenu"))
    Menu.setApplicationMenu(Menu.buildFromTemplate(template))
  }
}
