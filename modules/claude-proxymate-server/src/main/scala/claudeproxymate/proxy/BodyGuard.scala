package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import fs2.Stream
import org.http4s.Response

import scala.concurrent.duration.FiniteDuration

/** A response and the release of its client resource. */
final case class Forwarded(response: Response[IO], release: IO[Unit])

/** The body of a guarded response started after [[BodyGuard]] released the upstream transfer. */
final case class BodyStartedTooLate(startTimeout: FiniteDuration)
    extends RuntimeException(
      s"the response body started after $startTimeout, when the upstream transfer was already released"
    )

private enum BodyState derives CanEqual {
  case NotStarted
  case Started
  case Abandoned
}

/** Whether this side changed the state, and so owns the release. */
private enum Claim derives CanEqual {
  case Won
  case Lost
}

/** Releases the client resource of a response whose body is never read.
  *
  * The http4s Ember server writes the head of a response before it pulls the
  * body. When that write fails (the client is gone), or the connection stops
  * before the write, the body stream never starts, so a release put on the
  * body is never registered.
  *
  * The guard starts a watchdog. Exactly one of the body finalizer and the
  * watchdog runs the release: the body finalizer when the body stream starts
  * within `startTimeout`, else the watchdog, which then also runs
  * `onAbandoned`. A body that starts later, or a body that is run a second
  * time, fails with [[BodyStartedTooLate]].
  */
object BodyGuard {

  def apply(forwarded: Forwarded, startTimeout: FiniteDuration, onAbandoned: IO[Unit]): IO[Response[IO]] =
    for {
      state    <- Ref.of[IO, BodyState](BodyState.NotStarted)
      watchdog <- (IO.sleep(startTimeout) *> abandon(state).flatMap {
                    case Claim.Won =>
                      logged(forwarded.release, "the release of an unread response") *>
                        logged(onAbandoned, "the event for an unread response")
                    case Claim.Lost =>
                      IO.unit
                  }).start
    } yield {
      val body =
        Stream
          .bracket(claim(state)) {
            case Claim.Won => watchdog.cancel *> forwarded.release
            case Claim.Lost => IO.unit
          }
          .flatMap {
            case Claim.Won => forwarded.response.body
            case Claim.Lost => Stream.raiseError[IO](BodyStartedTooLate(startTimeout))
          }
      forwarded.response.withBodyStream(body)
    }

  /* NotStarted becomes Started and gives Won. Otherwise no change and Lost. */
  private def claim(state: Ref[IO, BodyState]): IO[Claim] =
    state.modify {
      case BodyState.NotStarted => (BodyState.Started, Claim.Won)
      case s @ (BodyState.Started | BodyState.Abandoned) => (s, Claim.Lost)
    }

  /* NotStarted becomes Abandoned and gives Won. Otherwise no change and Lost. */
  private def abandon(state: Ref[IO, BodyState]): IO[Claim] =
    state.modify {
      case BodyState.NotStarted => (BodyState.Abandoned, Claim.Won)
      case s @ (BodyState.Started | BodyState.Abandoned) => (s, Claim.Lost)
    }

  private def logged(io: IO[Unit], what: String): IO[Unit] =
    io.handleErrorWith { e =>
      IO(System.err.println(s"claude-proxymate: $what failed: ${errorMessage(e)}"))
    }

  private def errorMessage(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.getClass.getName)
}
