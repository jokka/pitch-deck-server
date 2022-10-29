package documents

import io.circe.Codec
import io.circe.generic.semiauto._
import rendering.Rendering

case class Page(aspectRatio: Double, src: Rendering[String], srcSet: Map[String, String], alt: String)

object Page {
  implicit lazy val pageCodec: Codec.AsObject[Page] = deriveCodec[Page]
}
