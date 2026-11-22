package claudeproxymate.renderer.state

import cats.syntax.all.*

import scala.scalajs.js

/** Pure functions for the list of proxy captures in [[AppState.proxyCaptures]].
  *
  * A capture is the request object that the proxy sends with `onProxyRequest`.
  * The response that the proxy sends with `onProxyResponse` has the same `id`.
  */
object ProxyCaptures {

  /** The renderer keeps only the newest `MaxCaptures` captures. */
  val MaxCaptures: Int = 50

  /** The id of a capture or a response.
    *
    * The capture id is a JSON number. A capture or a response without a
    * numeric id has no id, and so it matches nothing.
    */
  def idOf(capture: js.Dynamic): Option[Double] = {
    val raw = capture.selectDynamic("id")
    Option.when(js.typeOf(raw) === "number")(raw.asInstanceOf[Double]) // scalafix:ok DisableSyntax.asInstanceOf
  }

  /** Adds a captured request. The newest capture comes first, and the list
    * keeps at most [[MaxCaptures]] captures.
    */
  def addRequest(captures: List[js.Dynamic], request: js.Dynamic): List[js.Dynamic] =
    (request :: captures).take(MaxCaptures)

  /** Attaches a response to the capture with the same id.
    *
    * A response with an id that is not in the list, or without a numeric id,
    * changes nothing. The other captures stay the same objects.
    */
  def addResponse(captures: List[js.Dynamic], response: js.Dynamic): List[js.Dynamic] =
    idOf(response).fold(captures) { id =>
      captures.map(capture => if (idOf(capture).contains(id)) withResponse(capture, response) else capture)
    }

  /** A shallow copy of `capture` with `response`. The original capture does
    * not change.
    */
  private def withResponse(capture: js.Dynamic, response: js.Dynamic): js.Dynamic =
    js.Object
      .assign(
        js.Dynamic.literal(),
        capture.asInstanceOf[js.Object], // scalafix:ok DisableSyntax.asInstanceOf
        js.Dynamic.literal(response = response),
      )
      .asInstanceOf[js.Dynamic] // scalafix:ok DisableSyntax.asInstanceOf

  /** The capture with the id, or `None` when `id` is `None` or no capture has
    * the id.
    */
  def findById(captures: List[js.Dynamic], id: Option[Double]): Option[js.Dynamic] =
    id.flatMap(selected => captures.find(capture => idOf(capture).contains(selected)))
}
