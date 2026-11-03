package claudeproxymate.core.update

import cats.*
import cats.derived.strict.*
import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** The step of the self-update that a failure happened in. */
enum UpdatePhase derives CanEqual, Eq, Hash, Show {
  case Check
  case Download
  case Install
}
object UpdatePhase {

  extension (updatePhase: UpdatePhase) {

    def wire: String = updatePhase match {
      case UpdatePhase.Check => "check"
      case UpdatePhase.Download => "download"
      case UpdatePhase.Install => "install"
    }

  }

  def parse(s: String): Option[UpdatePhase] = s match {
    case "check" => UpdatePhase.Check.some
    case "download" => UpdatePhase.Download.some
    case "install" => UpdatePhase.Install.some
    case _ => none[UpdatePhase]
  }

  given Encoder[UpdatePhase] = Encoder.encodeString.contramap(_.wire)
  given Decoder[UpdatePhase] = Decoder.decodeString.emap(s => parse(s).toRight(s"Unknown update phase: $s"))
}

/** The self-update state that the Electron main process owns and pushes to the renderer.
  *
  * Times (`checkedAt`) are epoch milliseconds. `percent` is always within 0..100.
  */
enum UpdateState derives CanEqual, Eq, Show {
  case Unsupported
  case Idle
  case Checking(userInitiated: Boolean)
  case UpToDate(checkedAt: Double)
  case Available(version: String, checkedAt: Double)
  case Downloading(version: String, percent: Int)
  case Ready(version: String)
  case Failed(phase: UpdatePhase, message: String, userInitiated: Boolean, checkedAt: Option[Double])
}
object UpdateState {

  def unsupported: UpdateState                                   = UpdateState.Unsupported
  def idle: UpdateState                                          = UpdateState.Idle
  def checking(userInitiated: Boolean): UpdateState              = UpdateState.Checking(userInitiated)
  def upToDate(checkedAt: Double): UpdateState                   = UpdateState.UpToDate(checkedAt)
  def available(version: String, checkedAt: Double): UpdateState = UpdateState.Available(version, checkedAt)
  def downloading(version: String, percent: Int): UpdateState    = UpdateState.Downloading(version, percent)
  def ready(version: String): UpdateState                        = UpdateState.Ready(version)
  def failed(
    phase: UpdatePhase,
    message: String,
    userInitiated: Boolean,
    checkedAt: Option[Double],
  ): UpdateState = UpdateState.Failed(phase, message, userInitiated, checkedAt)

  /** A new check may start: nothing is in progress and no update waits for a restart. */
  def canCheck(state: UpdateState): Boolean = state match {
    case UpdateState.Checking(_) | UpdateState.Downloading(_, _) | UpdateState.Ready(_) | UpdateState.Unsupported =>
      false
    case UpdateState.Idle | UpdateState.UpToDate(_) | UpdateState.Available(_, _) | UpdateState.Failed(_, _, _, _) =>
      true
  }

  /** A download may start: an update was found, or the last download failed. */
  def canDownload(state: UpdateState): Boolean = state match {
    case UpdateState.Available(_, _) => true
    case UpdateState.Failed(phase, _, _, _) => phase === UpdatePhase.Download
    case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) |
         UpdateState.Downloading(_, _) | UpdateState.Ready(_) =>
      false
  }

  /** An install may start: the update is downloaded. */
  def canInstall(state: UpdateState): Boolean = state match {
    case UpdateState.Ready(_) => true
    case UpdateState.Unsupported | UpdateState.Idle | UpdateState.Checking(_) | UpdateState.UpToDate(_) |
         UpdateState.Available(_, _) | UpdateState.Downloading(_, _) | UpdateState.Failed(_, _, _, _) =>
      false
  }

  /** Rounds down and clamps a progress percentage to 0..100. NaN gives 0. */
  def clampPercent(d: Double): Int =
    if (d.isNaN) 0 else math.max(0, math.min(100, math.floor(d).toInt))

  /** The longest failure message [[displayMessage]] gives. */
  val MaxDisplayMessageLength: Int = 200

  /** A failure message short enough for the UI.
    *
    * electron-updater messages can contain a stack trace and all the HTTP response
    * headers (e.g. a missing `latest-mac.yml` on GitHub), so only the first line that
    * is not blank is kept, cut to [[MaxDisplayMessageLength]] with `…`. The full
    * message belongs in the log.
    */
  def displayMessage(raw: String): String =
    raw.split("\n").iterator.map(_.stripSuffix("\r").trim).find(_.nonEmpty) match {
      case Some(line) =>
        if (line.length > MaxDisplayMessageLength) s"${line.take(MaxDisplayMessageLength - 1).trim}…" else line
      case None =>
        "Unknown error"
    }

  given Encoder[UpdateState] = Encoder.instance {
    case UpdateState.Unsupported =>
      Json.obj("kind" -> "unsupported".asJson)
    case UpdateState.Idle =>
      Json.obj("kind" -> "idle".asJson)
    case UpdateState.Checking(userInitiated) =>
      Json.obj("kind" -> "checking".asJson, "userInitiated" -> userInitiated.asJson)
    case UpdateState.UpToDate(checkedAt) =>
      Json.obj("kind" -> "upToDate".asJson, "checkedAt" -> checkedAt.asJson)
    case UpdateState.Available(version, checkedAt) =>
      Json.obj("kind" -> "available".asJson, "version" -> version.asJson, "checkedAt" -> checkedAt.asJson)
    case UpdateState.Downloading(version, percent) =>
      Json.obj("kind" -> "downloading".asJson, "version" -> version.asJson, "percent" -> percent.asJson)
    case UpdateState.Ready(version) =>
      Json.obj("kind" -> "ready".asJson, "version" -> version.asJson)
    case UpdateState.Failed(phase, message, userInitiated, checkedAt) =>
      Json.obj(
        "kind"          -> "failed".asJson,
        "phase"         -> phase.asJson,
        "message"       -> message.asJson,
        "userInitiated" -> userInitiated.asJson,
        "checkedAt"     -> checkedAt.asJson,
      )
  }

  given Decoder[UpdateState] = Decoder.instance { c =>
    c.get[String]("kind").flatMap {
      case "unsupported" =>
        unsupported.asRight
      case "idle" =>
        idle.asRight
      case "checking" =>
        c.get[Boolean]("userInitiated").map(checking)
      case "upToDate" =>
        c.get[Double]("checkedAt").map(upToDate)
      case "available" =>
        for {
          version   <- c.get[String]("version")
          checkedAt <- c.get[Double]("checkedAt")
        } yield available(version, checkedAt)
      case "downloading" =>
        for {
          version <- c.get[String]("version")
          percent <- c.get[Int]("percent")
        } yield downloading(version, percent)
      case "ready" =>
        c.get[String]("version").map(ready)
      case "failed" =>
        for {
          phase         <- c.get[UpdatePhase]("phase")
          message       <- c.get[String]("message")
          userInitiated <- c.get[Boolean]("userInitiated")
          checkedAt     <- c.get[Option[Double]]("checkedAt")
        } yield failed(phase, message, userInitiated, checkedAt)
      case other =>
        io.circe.DecodingFailure(s"Unknown update state kind: $other", c.history).asLeft
    }
  }
}
