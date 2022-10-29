package documents

import cats.implicits._
import cats.effect._
import cats.effect.std._
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.file._
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{deriveConfiguredDecoder, deriveConfiguredEncoder}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import random.RandomId
import rendering.Rendering

import scala.concurrent.duration.DurationInt

trait Documents[F[_]] {
  def watchDocuments: F[(Seq[Document], Stream[F, Documents.Event])]
  def getDocuments: F[Seq[Document]]
  def createDocument(fileName: String, file: Stream[F, Byte]): F[Document]
}

object Documents {
  private val baseImageWidth = 768

  def apply[F[_]: Async: Random](tempDir: Path): F[Documents[F]] = for {
    semaphore    <- Semaphore[F](1)
    documentsRef <- Ref[F].of(Seq.empty[Document])
    eventsTopic  <- Topic[F, Event]
    beforeRender <-
      Sync[F].delay {
        sys.env.get("IMAGE_RENDERING_DELAY").map(_.toInt.millis).map(Async[F].sleep).getOrElse(Async[F].unit)
      }.recoverWith { case _: Throwable =>
        Slf4jLogger
          .getLogger[F]
          .warn("Received invalid IMAGE_RENDERING_DELAY. It should be an int, representing delay in milliseconds")
          .map(_ => Async[F].unit)
      }
  } yield new Documents[F] {
    override def watchDocuments: F[(Seq[Document], Stream[F, Event])] =
      semaphore.permit.use { _ =>
        documentsRef.get.map { documents =>
          documents -> eventsTopic.subscribe(1024)
        }
      }

    override def getDocuments: F[Seq[Document]] = documentsRef.get

    override def createDocument(fileName: String, file: Stream[F, Byte]): F[Document] = for {
      documentId <- RandomId[F]()
      documentDirectory = tempDir / documentId
      documentPath      = documentDirectory / fileName
      _             <- Files[F].createDirectories(documentDirectory)
      _             <- file.through(Files[F].writeAll(documentPath)).compile.drain
      pagesDeferred <- Deferred[F, Seq[Page]]
      _ <- Async[F].start {
        rendering.pdfDocument[F](documentPath, beforeRender).use { pdfPages =>
          for {
            _ <- pagesDeferred.complete {
              pdfPages.map { pdfPage =>
                Page(
                  pdfPage.aspectRatio,
                  Rendering.Pending,
                  Map.empty,
                  s"Page ${pdfPage.index + 1} of $fileName"
                )
              }
            }
            renderPages = (constraint: Int) =>
              (
                doBefore: Int => F[Unit],
                doAfter: (Int, Rendering[String]) => F[Unit]
              ) =>
                Stream
                  .emits(pdfPages)
                  .parEvalMap[F, Unit](Runtime.getRuntime.availableProcessors) { pdfPage =>
                    val pageDirectory = documentDirectory / pdfPage.index.toString
                    val imagePath     = pageDirectory / s"${constraint}x.png"

                    for {
                      _ <- doBefore(pdfPage.index)
                      rendering <- pdfPage
                        .renderPng(constraint * baseImageWidth, imagePath)
                        .map[Rendering[String]](_ => Rendering.Completed("/" + tempDir.relativize(imagePath)))
                        .recoverWith { case t: Throwable =>
                          Slf4jLogger
                            .getLogger[F]
                            .warn("Failed to render image " + t)
                            .map(_ => Rendering.Failed("Failed to render image"))
                        }
                      _ <- doAfter(pdfPage.index, rendering)
                      _ <- rendering match {
                        case Rendering.Completed(src) =>
                          addSrcSetEntry(documentId, pdfPage.index, s"${constraint}x" -> src)
                        case _ => Async[F].unit
                      }
                    } yield ()
                  }
                  .compile
                  .drain
            _ <- renderPages(1)(
              index =>
                updatePageSrc(documentId, index, Rendering.InProgress(None)) *> Files[F]
                  .createDirectories(documentDirectory / index.toString),
              (index, rendering) => updatePageSrc(documentId, index, rendering)
            )
            _ <- renderPages(2)(
              _ => Async[F].unit,
              (_, _) => Async[F].unit
            )
            _ <- renderPages(3)(
              _ => Async[F].unit,
              (_, _) => Async[F].unit
            )
          } yield ()
        }
      }
      pages <- pagesDeferred.get
      document = Document(documentId, fileName, pages)
      _ <- semaphore.permit.use { _ =>
        documentsRef.update(_ :+ document) *> eventsTopic.publish1(DocumentCreated(document))
      }
    } yield document

    private def updatePageSrc(documentId: String, pageIndex: Int, src: Rendering[String]): F[Unit] =
      semaphore.permit.use { _ =>
        for {
          _ <- updatePage(documentId, pageIndex) { prevPage =>
            prevPage.copy(
              src = src
            )
          }
          _ <- eventsTopic.publish1(PageSrcUpdated(documentId, pageIndex, src))
        } yield ()
      }

    private def addSrcSetEntry(documentId: String, pageIndex: Int, entry: (String, String)): F[Unit] =
      semaphore.permit.use { _ =>
        for {
          _ <- updatePage(documentId, pageIndex) { prevPage =>
            prevPage.copy(
              srcSet = prevPage.srcSet + entry
            )
          }
          _ <- eventsTopic.publish1(SrcSetEntryAdded(documentId, pageIndex, entry))
        } yield ()
      }

    private def updatePage(documentId: String, pageIndex: Int)(fn: Page => Page): F[Unit] = documentsRef.update {
      documents =>
        documents.map { document =>
          if (document.id == documentId) {
            val prevPage = document.pages(pageIndex)
            val nextPage = fn(prevPage)
            document.copy(
              pages = document.pages.updated(pageIndex, nextPage)
            )
          } else {
            document
          }
        }
    }
  }

  sealed trait Event
  case class DocumentCreated(document: Document)                                           extends Event
  case class PageSrcUpdated(documentId: String, pageIndex: Int, src: Rendering[String])    extends Event
  case class SrcSetEntryAdded(documentId: String, pageIndex: Int, entry: (String, String)) extends Event

  object Event {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("is")

    implicit lazy val eventEncoder: Encoder.AsObject[Event] = deriveConfiguredEncoder[Event]
    implicit lazy val eventDecoder: Decoder[Event]          = deriveConfiguredDecoder[Event]
  }
}
