package server

import auth.{Session, Sessions}
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import cats.effect._
import cats.effect.std.Random
import com.comcast.ip4s._
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.{AuthedRoutes, Credentials, HttpRoutes, MediaType, Request}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.Multipart
import org.http4s.server.middleware.CORS
import documents.Documents
import org.http4s.AuthScheme.Bearer
import org.http4s.server.AuthMiddleware
import org.http4s.server.staticcontent.{fileService, FileService}

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { implicit random =>
    Sessions[IO].use { sessions =>
      Documents[IO].use { documents =>
        run[IO](sessions, documents)
      }
    }
  }

  def run[F[_]: Async](sessions: Sessions[F], documents: Documents[F]): F[ExitCode] = for {
    tempDirectory <- documents.getTempDirectory

    staticFilesService = fileService[F](FileService.Config(tempDirectory.absolute.toString, pathPrefix = "static"))
    sessionsService    = sessionRoutes(sessions)
    requireSession     = sessionMiddleware(sessions)
    documentsService   = requireSession(documentRoutes(documents))

    server <- EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"9000")
      .withHttpApp(cors(staticFilesService <+> sessionsService <+> documentsService).orNotFound)
      .build
      .use(_ => Async[F].never[Unit])
      .as(ExitCode.Success)
  } yield server

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
      case GET -> Root / "session" =>
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

  private def documentRoutes[F[_]: Async](documents: Documents[F]): AuthedRoutes[Session, F] = {
    object dsl extends Http4sDsl[F]

    import dsl._

    AuthedRoutes.of[Session, F] {
      case GET -> Root / "documents" as session =>
        for {
          documents <- documents.getDocuments(session)
          response  <- Ok(documents.asJson)
        } yield response
      case GET -> Root / "documents" / id as session =>
        for {
          documents <- documents.getDocuments(session)
          response <- documents.find(_.id == id) match {
            case Some(document) => Ok(document.asJson)
            case None           => NotFound(Json.obj("error" -> "Document not found".asJson))
          }
        } yield response
      case auth @ POST -> Root / "documents" as session =>
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
                document <- documents.createDocument(fileName, part.body, session)
                response <- Ok(document.asJson)
              } yield response
            }
          }
        }
    }
  }

  private val cors = CORS.policy.withAllowOriginAll
    .withAllowCredentials(false)
}
