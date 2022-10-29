package random

import cats.implicits._
import cats.effect.Sync
import cats.effect.std.Random
import fs2.Stream

import scala.collection.mutable

object RandomId {
  def apply[F[_]: Sync: Random](length: Int = 10): F[String] = Stream
    .range(0, length)
    .evalMap(_ => Random[F].nextAlphaNumeric)
    .fold(new mutable.StringBuilder)(_ append _)
    .take(1)
    .compile
    .lastOrError
    .map(_.toString())
}
