package documents

import io.circe.Codec
import io.circe.generic.semiauto._

case class Document(id: String, fileName: String, pages: Seq[Page])

object Document {
  implicit lazy val documentCodec: Codec.AsObject[Document] = deriveCodec[Document]
}
