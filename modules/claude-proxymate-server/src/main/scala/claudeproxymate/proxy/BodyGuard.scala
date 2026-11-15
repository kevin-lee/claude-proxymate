package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.syntax.all.*
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
      watchdog <- (IO.sleep(startTimeout) *> abandon(state).flatMap { abandoned =>
                    (logged(forwarded.release, "the release of an unread response") *>
                      logged(onAbandoned, "the event for an unread response")).whenA(abandoned)
                  }).start
    } yield {
      val body =
        Stream
          .bracket(claim(state))(claimed => (watchdog.cancel *> forwarded.release).whenA(claimed))
          .flatMap { claimed =>
            if (claimed) forwarded.response.body
            else Stream.raiseError[IO](BodyStartedTooLate(startTimeout))
          }
      forwarded.response.withBodyStream(body)
    }

  /* NotStarted becomes Started and gives true. Otherwise no change and false. */
  private def claim(state: Ref[IO, BodyState]): IO[Boolean] =
    state.modify {
      case BodyState.NotStarted => (BodyState.Started, true)
      case s @ (BodyState.Started | BodyState.Abandoned) => (s, false)
    }

  /* NotStarted becomes Abandoned and gives true. Otherwise no change and false. */
  private def abandon(state: Ref[IO, BodyState]): IO[Boolean] =
    state.modify {
      case BodyState.NotStarted => (BodyState.Abandoned, true)
      case s @ (BodyState.Started | BodyState.Abandoned) => (s, false)
    }

  private def logged(io: IO[Unit], what: String): IO[Unit] =
    io.handleErrorWith { e =>
      IO(System.err.println(s"claude-proxymate: $what failed: ${errorMessage(e)}"))
    }

  private def errorMessage(e: Throwable): String =
    Option(e.getMessage).getOrElse(e.getClass.getName)
}
