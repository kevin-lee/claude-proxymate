package claudeproxymate.core

import refined4s.Newtype
import refined4s.modules.circe.derivation.CirceNewtypeCodec

/** An instant as milliseconds since 1970-01-01T00:00:00Z.
  *
  * It has no time zone. In JSON, it is a number.
  */
type EpochMillis = EpochMillis.Type

object EpochMillis extends Newtype[Long], CirceNewtypeCodec[Long]
