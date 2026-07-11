package claudeproxymate.electron.facades

import cats.syntax.all.*

import scala.scalajs.js

/** The subset of the `jsonc-parser` npm module used by VsCodeSync. */
@js.native
trait JsoncParserModule extends js.Object {
  def parse(text: String, errors: js.Array[js.Dynamic], options: js.Object): js.Any = js.native
  def modify(text: String, path: js.Array[js.Any], value: js.Any, options: js.Object): js.Array[js.Dynamic] =
    js.native
  def applyEdits(text: String, edits: js.Array[js.Dynamic]): String = js.native
  def stripComments(text: String, replaceCh: String): String        = js.native
}

/** `jsonc-parser` loaded via a dynamic `require` in try/catch.
  *
  * A static `@JSImport` would emit a top-level `require("jsonc-parser")`
  * in the CommonJS bundle and kill the whole main process at startup
  * when `electron-app/node_modules` is stale (e.g. `devUi` without
  * `npm install`). With the dynamic load, only the VS Code sync feature
  * degrades — the app keeps working.
  *
  * `require` is module-scoped in Node, not a global, so the lookup goes
  * through `process.mainModule.require` (set for the CJS entry module)
  * with `globalThis.require` as a first attempt for environments that do
  * expose it.
  */
object JsoncParser {

  val MissingModuleMessage: String = "jsonc-parser module missing - run `npm install` in electron-app"

  lazy val get: Option[JsoncParserModule] =
    attempt(() => js.Dynamic.global.selectDynamic("require"))
      .orElse(attempt(() => js.Dynamic.global.process.selectDynamic("mainModule").selectDynamic("require")))

  private def attempt(requireFn: () => js.Dynamic): Option[JsoncParserModule] =
    try {
      val fn = requireFn()
      if (js.isUndefined(fn) || fn == null) {
        none[JsoncParserModule]
      } else {
        Option(fn("jsonc-parser")).map(m => m.asInstanceOf[JsoncParserModule])
      }
    } catch {
      case _: Throwable => none[JsoncParserModule]
    }
}
