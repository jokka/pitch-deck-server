package server

import auth.{Session, Sessions}
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import cats.effect._
import cats.effect.std.Random
import com.comcast.ip4s._
import io.circe.Json
import io.circe.syntax._
import org.http4s.{AuthedRoutes, Credentials, HttpRoutes, MediaType, Request}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.Multipart
import org.http4s.server.middleware.CORS
import documents.Buckets
import fs2.io.file.{Files, Path}
import org.http4s.AuthScheme.Bearer
import org.http4s.server.AuthMiddleware
import org.http4s.server.staticcontent.{fileService, FileService}
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { implicit random =>
    Files[IO].tempDirectory.use { tempDir =>
      for {
        sessions <- Sessions[IO]
        buckets  <- Buckets[IO, Session](tempDir)
        serverContext = ServerContext(tempDir, sessions, buckets)
        exitCode <- run[IO](serverContext)
      } yield exitCode
    }
  }

  case class ServerContext[F[_]](tempDir: Path, sessions: Sessions[F], buckets: Buckets[F, Session])

  def run[F[_]: Async](serverContext: ServerContext[F]): F[ExitCode] = {
    val ServerContext(tempDir, sessions, buckets) = serverContext
    val staticFilesService =
      fileService[F](FileService.Config(tempDir.absolute.toString, pathPrefix = "static"))
    val sessionsService = sessionRoutes(sessions)
    val eventsService   = eventRoutes(buckets)(_)
    val documentService = documentRoutes(buckets)
    val requireSession  = sessionMiddleware(sessions)

    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"9000")
      .withHttpWebSocketApp { ws =>
        cors(
          staticFilesService <+> sessionsService <+> eventsService(ws) <+> requireSession(documentService)
        ).orNotFound
      }
      .build
      .use(_ => Async[F].never[Unit])
      .as(ExitCode.Success)
  }

  private def sessionMiddleware[F[_]: Async](sessions: Sessions[F]): AuthMiddleware[F, Session] =
    AuthMiddleware[F, Session] {
      Kleisli { req: Request[F] =>
        for {
          authorization <- OptionT.fromOption[F](req.headers.get[Authorization])
          session <- authorization.credentials match {
            case Credentials.Token(scheme, token) if scheme == Bearer =>
              OptionT(sessions.getSession(token))
            case _ =>
              OptionT.none[F, Session]
          }
        } yield session
      }
    }

  private def sessionRoutes[F[_]: Async](sessions: Sessions[F]): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F]

    import dsl._

    HttpRoutes.of[F] {
      case POST -> Root / "session" =>
        for {
          session  <- sessions.createSession
          response <- Ok(session.asJson)
        } yield response
      case GET -> Root / "session" / sessionToken =>
        for {
          sessionExists <- sessions.verifySession(sessionToken)
          response      <- if (sessionExists) Ok() else NotFound()
        } yield response
    }
  }

  private def eventRoutes[F[_]: Async](
    buckets: Buckets[F, Session]
  )(ws: WebSocketBuilder2[F]): HttpRoutes[F] = {
    object dsl extends Http4sDsl[F]

    import dsl._
    import fs2._

    HttpRoutes.of[F] {
      // This is a WebSocket endpoint which interprets incoming messages as session tokens
      // and streams back document events related to last received token
      case GET -> Root / "events" =>
        ws.build {
          _.flatMap {
            case WebSocketFrame.Text((message, _)) => Stream.emit(message)
            case _                                 => Stream.empty
          }.switchMap { sessionToken =>
            val session = Session(sessionToken)
            buckets.watchBucket(session).switchMap {
              case Some(bucket) =>
                for {
                  documents          <- Stream.eval(bucket.documents)
                  (snapshot, events) <- Stream.eval(documents.watchDocuments)
                  jsonEvent <- Stream
                    .emit(Json.obj("is" -> "Snapshot".asJson, "documents" -> snapshot.asJson))
                    .append(events.map(_.asJson))
                } yield WebSocketFrame.Text(jsonEvent.noSpaces)
              case None =>
                Stream.empty
            }
          }
        }
    }
  }

  private def documentRoutes[F[_]: Async](
    buckets: Buckets[F, Session]
  ): AuthedRoutes[Session, F] = {
    object dsl extends Http4sDsl[F]

    import dsl._

    AuthedRoutes.of[Session, F] {
      case GET -> Root / "documents" as session =>
        for {
          bucket    <- buckets.getOrCreateBucket(session)
          documents <- bucket.documents
          document  <- documents.getDocuments
          response  <- Ok(document.asJson)
        } yield response
      case GET -> Root / "documents" / id as session =>
        for {
          bucket    <- buckets.getOrCreateBucket(session)
          documents <- bucket.documents
          docs      <- documents.getDocuments
          response <- docs.find(_.id == id) match {
            case Some(document) => Ok(document.asJson)
            case None           => NotFound(Json.obj("error" -> "Document not found".asJson))
          }
        } yield response
      case auth @ POST -> Root / "documents" as session =>
        buckets.getOrCreateBucket(session).flatMap { bucket =>
          auth.req.decode[Multipart[F]] { multipart =>
            if (multipart.parts.length != 1) {
              BadRequest(Json.obj("error" -> "Malformed form data".asJson))
            } else {
              val part               = multipart.parts.head
              val contentType        = part.headers.get[`Content-Type`]
              val contentDisposition = part.headers.get[`Content-Disposition`]

              if (!contentType.exists(_.mediaType == MediaType.application.pdf)) {
                BadRequest(Json.obj("error" -> "Malformed form data".asJson))
              } else {
                val fileName = contentDisposition.flatMap(_.filename).getOrElse("unnamed.pdf")

                for {
                  documents <- bucket.documents
                  document  <- documents.createDocument(fileName, part.body)
                  response  <- Ok(document.asJson)
                } yield response
              }
            }
          }
        }
    }
  }

  private val cors = CORS.policy.withAllowOriginAll
    .withAllowCredentials(false)
}
