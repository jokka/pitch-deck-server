package auth

import io.circe.Codec
import io.circe.generic.semiauto._

case class Session(sessionToken: String)

object Session {
  implicit lazy val sessionCodec: Codec.AsObject[Session] = deriveCodec[Session]
}
