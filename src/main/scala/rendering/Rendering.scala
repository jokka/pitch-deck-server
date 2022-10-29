package rendering

import io.circe._
import io.circe.syntax.EncoderOps

sealed trait Rendering[+T]

object Rendering {
  case object Pending                            extends Rendering[Nothing]
  case class InProgress(progress: Option[Float]) extends Rendering[Nothing]
  case class Completed[T](value: T)              extends Rendering[T]
  case class Failed(message: String)             extends Rendering[Nothing]

  implicit def renderingEncoder[T: Encoder]: Encoder[Rendering[T]] = {
    case Pending              => Json.obj("is" -> "Pending".asJson)
    case InProgress(progress) => Json.obj("is" -> "InProgress".asJson, "progress" -> progress.asJson)
    case Completed(value)     => Json.obj("is" -> "Completed".asJson, "value" -> value.asJson)
    case Failed(message)      => Json.obj("is" -> "Failed".asJson, "message" -> message.asJson)
  }

  implicit def renderingDecoder[T: Decoder]: Decoder[Rendering[T]] = hcursor =>
    hcursor.downField("is").as[String].flatMap {
      case "Pending"    => Right(Pending)
      case "InProgress" => hcursor.downField("progress").as[Option[Float]].map(progress => InProgress(progress))
      case "Completed"  => hcursor.downField("value").as[T].map(value => Completed(value))
      case "Failed"     => hcursor.downField("message").as[String].map(message => Failed(message))
      case other        => Left(DecodingFailure(s"$other is not valid Rendering is", Nil))
    }
}
