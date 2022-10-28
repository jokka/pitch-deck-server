package documents

import auth.Session
import cats.implicits._
import cats.effect.{Async, Ref, Sync}
import cats.effect.kernel.Resource
import cats.effect.std.Random
import fs2.Stream
import fs2.io.file.{Files, Path}
import random.RandomId

trait Documents[F[_]] {
  def getTempDirectory: F[Path]
  def getDocuments(owner: Session): F[Seq[Document]]
  def createDocument(fileName: String, file: Stream[F, Byte], owner: Session): F[Document]
}

object Documents {
  private val baseUrl = "http://localhost:9000/static"

  private val baseImageWidth = 768

  def apply[F[_]: Async: Random]: Resource[F, Documents[F]] = for {
    tempDirectory <- Files[F].tempDirectory
    documentsRef  <- Resource.eval(Ref[F].of(Map.empty[Session, Seq[DocumentRecord]]))
  } yield new Documents[F] {
    override def getTempDirectory: F[Path] = Sync[F].pure(tempDirectory)

    override def getDocuments(owner: Session): F[Seq[Document]] = for {
      documentsByOwner <- documentsRef.get
    } yield documentsByOwner.getOrElse(owner, List.empty).map(_.document)

    override def createDocument(fileName: String, file: Stream[F, Byte], owner: Session): F[Document] = for {
      documentId <- RandomId[F]()
      documentDirectory = tempDirectory / documentId
      documentPath      = documentDirectory / fileName
      _ <- Files[F].createDirectories(documentDirectory)
      _ <- file.through(Files[F].writeAll(documentPath)).compile.drain
      pages <- rendering.pdfDocument[F](documentPath).use { pages =>
        Stream
          .emits(pages)
          .evalMap { page =>
            val pageDirectory = documentDirectory / page.index.toString
            val imagePath1x   = pageDirectory / "1x.png"
            val imagePath2x   = pageDirectory / "2x.png"
            val imagePath3x   = pageDirectory / "3x.png"

            for {
              _ <- Files[F].createDirectories(pageDirectory)
              _ <- page.renderPng(baseImageWidth, imagePath1x)
              _ <- page.renderPng(2 * baseImageWidth, imagePath2x)
              _ <- page.renderPng(3 * baseImageWidth, imagePath3x)
            } yield Page(
              page.aspectRatio,
              Image(
                baseUrl + "/" + tempDirectory.relativize(imagePath1x),
                Map(
                  "1x" -> (baseUrl + "/" + tempDirectory.relativize(imagePath1x)),
                  "2x" -> (baseUrl + "/" + tempDirectory.relativize(imagePath2x)),
                  "3x" -> (baseUrl + "/" + tempDirectory.relativize(imagePath3x))
                ),
                s"Page ${page.index + 1} of $fileName"
              )
            )
          }
          .compile
          .toList
      }
      document = Document(documentId, fileName, pages)
      _ <- documentsRef.update { documentsByOwner =>
        documentsByOwner.updatedWith(owner) { prev =>
          Some(prev.getOrElse(Seq.empty) :+ DocumentRecord(document, owner))
        }
      }
    } yield document
  }
}
