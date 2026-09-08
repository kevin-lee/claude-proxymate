package claudeproxymate.proxy

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import claudeproxymate.core.filter.FilterConfig
import io.circe.parser.decode

import java.nio.file.{Files, Paths}

/** Reads the request filter config file on every request.
  *
  * The Electron main process owns the file (`<userData>/request-filter.json`)
  * and rewrites it on Save, so re-reading per request is what makes edits apply
  * without restarting the proxy. The file is tiny; parsing is skipped when the
  * text is unchanged since the last read. A missing or unreadable file, or an
  * invalid one, yields [[FilterConfig.disabled]] so the proxy keeps forwarding.
  *
  * Diagnostics go to stderr only: stdout is the JSON-lines event channel
  * (see [[EventEmitter]]).
  */
final class FilterConfigLoader(path: Option[String], cache: Ref[IO, Option[(String, FilterConfig)]]) {

  def load: IO[FilterConfig] =
    path match {
      case None => IO.pure(FilterConfig.disabled)
      case Some(p) =>
        readText(p).flatMap {
          case None => IO.pure(FilterConfig.disabled)
          case Some(text) =>
            cache.get.flatMap {
              case Some((cachedText, cachedConfig)) if cachedText === text => IO.pure(cachedConfig)
              case _ =>
                val config = FilterConfigLoader.parse(text) match {
                  case Right(cfg) => IO.pure(cfg)
                  case Left(err) =>
                    IO(System.err.println(s"claude-proxymate: ignoring invalid filter config at $p: $err"))
                      .as(FilterConfig.disabled)
                }
                config.flatTap(cfg => cache.set((text, cfg).some))
            }
        }
    }

  private def readText(p: String): IO[Option[String]] =
    IO.blocking(Files.readString(Paths.get(p))).map(_.some).handleError(_ => none[String])
}

object FilterConfigLoader {

  /** Pure decode of the file text, unit-tested; every I/O concern lives in [[FilterConfigLoader.load]]. */
  def parse(text: String): Either[String, FilterConfig] =
    decode[FilterConfig](text).left.map(_.getMessage)

  def make(path: Option[String]): IO[FilterConfigLoader] =
    Ref.of[IO, Option[(String, FilterConfig)]](none[(String, FilterConfig)]).map(new FilterConfigLoader(path, _))
}
