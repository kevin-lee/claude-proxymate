package claudeproxymate.electron

import claudeproxymate.electron.facades.{NodeCrypto, NodeFs, NodeHttps, NodePath}

import java.util.concurrent.atomic.AtomicReference
import scala.scalajs.js
import scala.scalajs.js.JSON

/** Google Analytics 4 integration. Ports analytics.js. */
object Analytics {

  private val MeasurementId = sys.env.getOrElse("GA_MEASUREMENT_ID", "")
  private val ApiSecret     = sys.env.getOrElse("GA_API_SECRET", "")

  final private case class State(
    clientId: Option[String],
    userDataPath: String,
    sessionId: Option[String],
  )

  private val state = new AtomicReference[State](State(None, "", None))

  def init(path: String): Unit = {
    val sid = js.Date.now().toLong.toString
    state.updateAndGet(s => s.copy(userDataPath = path, sessionId = Some(sid))): Unit
  }

  private def getClientId(): String = {
    val current = state.get()
    current.clientId match {
      case Some(id) => id
      case None =>
        val filePath = NodePath.join(current.userDataPath, "analytics.json")
        val id       = readOrCreateClientId(filePath)
        state.updateAndGet(s => s.copy(clientId = Some(id))): Unit
        id
    }
  }

  private def readOrCreateClientId(filePath: String): String = {
    val existing =
      if (NodeFs.existsSync(filePath)) {
        try {
          val data   = NodeFs.readFileSync(filePath, "utf8")
          val parsed = JSON.parse(data)
          Option(parsed.selectDynamic("clientId").asInstanceOf[String]).filter(_.nonEmpty)
        } catch {
          case _: Throwable => None
        }
      } else {
        None
      }

    existing.getOrElse {
      val newId = NodeCrypto.randomUUID()
      try {
        NodeFs.writeFileSync(filePath, JSON.stringify(js.Dynamic.literal(clientId = newId)))
      } catch {
        case _: Throwable => () // silently ignore write errors
      }
      newId
    }
  }

  def trackEvent(eventName: String): Unit =
    trackEvent(eventName, js.Dictionary.empty)

  def trackEvent(eventName: String, params: js.Dictionary[js.Any]): Unit = {
    if (MeasurementId.isEmpty || ApiSecret.isEmpty) {
      ()
    } else {
      val eventParams = js.Dictionary[js.Any](
        "session_id"           -> state.get().sessionId.getOrElse(""),
        "engagement_time_msec" -> 100,
      )
      params.foreach { case (k, v) => eventParams(k) = v }

      val body = JSON.stringify(
        js.Dynamic
          .literal(
            client_id = getClientId(),
            events = js.Array(
              js.Dynamic
                .literal(
                  name = eventName,
                  params = eventParams,
                )
            ),
          )
      )

      val options = js
        .Dynamic
        .literal(
          hostname = "www.google-analytics.com",
          path = s"/mp/collect?measurement_id=$MeasurementId&api_secret=$ApiSecret",
          method = "POST",
          headers = js
            .Dynamic
            .literal(
              `Content-Type` = "application/json",
            ),
        )

      try {
        val req = NodeHttps.request(options.asInstanceOf[js.Object])
        req.on("error", { (_: js.Any) => () }: js.Function1[js.Any, Unit])
        req.write(body): Unit
        req.end()
      } catch {
        case _: Throwable => () // silently ignore
      }
    }
  }
}
