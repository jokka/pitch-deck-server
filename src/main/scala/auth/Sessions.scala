package auth

import cats.implicits._
import cats.effect._
import cats.effect.std.Random
import random.RandomId

trait Sessions[F[_]] {
  def createSession: F[Session]
  def getSession(sessionToken: String): F[Option[Session]]
  def verifySession(sessionToken: String): F[Boolean]
}

object Sessions {
  def apply[F[_]: Sync: Random]: F[Sessions[F]] = for {
    sessionsRef <- Ref[F].of(Set.empty[Session])
  } yield new Sessions[F] {
    override def createSession: F[Session] = for {
      sessionToken <- RandomId[F](128)
      session = Session(sessionToken)
      _ <- sessionsRef.update(sessions => sessions + session)
    } yield session

    override def getSession(sessionToken: String): F[Option[Session]] = for {
      sessions <- sessionsRef.get
      session = Session(sessionToken)
    } yield Some(session).filter(sessions)

    override def verifySession(sessionToken: String): F[Boolean] = for {
      sessions <- sessionsRef.get
    } yield sessions(Session(sessionToken))
  }
}
