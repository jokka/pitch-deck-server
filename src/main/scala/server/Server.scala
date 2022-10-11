package server

import cats.implicits._
import cats.effect._
import cats.effect.std.Random
import com.comcast.ip4s._
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.{HttpRoutes, MediaType}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart.Multipart
import org.http4s.server.middleware.CORS
import documents.Documents
import org.http4s.server.staticcontent.{fileService, FileService}

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { implicit random =>
    Documents[IO].use(run[IO])
  }

  def run[F[_]: Async](documents: Documents[F]): F[ExitCode] = for {
    tempDirectory <- documents.getTempDirectory

    staticFilesService = fileService[F](FileService.Config(tempDirectory.absolute.toString, pathPrefix = "static"))
    documentsService   = routes(documents)

    server <- EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"9000")
      .withHttpApp(cors(staticFilesService <+> documentsService).orNotFound)
      .build
      .use(_ => Async[F].never[Unit])
      .as(ExitCode.Success)
  } yield server

  private def routes[F[_]: Async](service: Documents[F]) = {
    object dsl extends Http4sDsl[F]

    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "documents" =>
        for {
          documents <- service.getDocuments
          response  <- Ok(documents.asJson)
        } yield response
      case GET -> Root / "documents" / id =>
        for {
          documents <- service.getDocuments
          response <- documents.find(_.id == id) match {
            case Some(document) => Ok(document.asJson)
            case None           => NotFound(Json.obj("error" -> "Document not found".asJson))
          }
        } yield response
      case req @ POST -> Root / "documents" =>
        req.decode[Multipart[F]] { multipart =>
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
                document <- service.createDocument(fileName, part.body)
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
