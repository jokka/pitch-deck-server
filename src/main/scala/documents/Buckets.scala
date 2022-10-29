package documents

import cats.implicits._
import cats.effect._
import cats.effect.std.{Random, Semaphore}
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.file.Path

trait Bucket[F[_]] {
  def documents: F[Documents[F]]
}

trait Buckets[F[_], Owner] {
  def watchBucket(owner: Owner): Stream[F, Option[Bucket[F]]]
  def getOrCreateBucket(owner: Owner): F[Bucket[F]]
}

object Buckets {
  def apply[F[_]: Async: Random, Owner](tempDir: Path): F[Buckets[F, Owner]] = for {
    semaphore   <- Semaphore[F](1)
    bucketsRef  <- Ref[F].of(Map.empty[Owner, Bucket[F]])
    eventsTopic <- Topic[F, Event[Owner]]
  } yield new Buckets[F, Owner] {
    override def watchBucket(owner: Owner): Stream[F, Option[Bucket[F]]] =
      for {
        (buckets, events) <- Stream.eval(semaphore.permit.use { _ =>
          bucketsRef.get.map(_ -> eventsTopic.subscribe(1024))
        })
        bucket <- Stream
          .emit(buckets.get(owner))
          .append(events.filter(_.owner == owner).evalMap(_ => bucketsRef.get.map(_.get(owner))))
      } yield bucket

    override def getOrCreateBucket(owner: Owner): F[Bucket[F]] =
      semaphore.permit.use { _ =>
        for {
          buckets <- bucketsRef.get
          bucket <- buckets.get(owner) match {
            case Some(bucket) => Async[F].pure(bucket)
            case None =>
              for {
                docs <- Documents[F](tempDir)
                bucket = new Bucket[F] {
                  override val documents: F[Documents[F]] = Async[F].pure(docs)
                }
                _ <- bucketsRef.set(buckets + (owner -> bucket))
                _ <- eventsTopic.publish1(BucketCreated(owner))
              } yield bucket
          }
        } yield bucket
      }
  }

  sealed trait Event[Owner] {
    def owner: Owner
  }
  case class BucketCreated[Owner](owner: Owner) extends Event[Owner]
}
