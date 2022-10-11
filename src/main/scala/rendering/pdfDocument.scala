package rendering

import cats.effect.Sync
import cats.effect.kernel.Resource
import fs2.io.file.Path
import javax.imageio.ImageIO
import org.apache.pdfbox.io.RandomAccessFile
import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.PDFRenderer

object pdfDocument {
  def apply[F[_]: Sync](documentPath: Path): Resource[F, Seq[pdfPage[F]]] =
    pdDocument(documentPath).map { document =>
      fs2.Stream
        .range(0, document.getNumberOfPages)
        .map { i =>
          val page = document.getPage(i)
          val box  = page.getMediaBox

          new pdfPage[F] {
            override val index: Int = i

            override val aspectRatio: Double = box.getWidth.toDouble / box.getHeight

            override def renderPng(width: Int, filePath: Path): F[Unit] = Sync[F].blocking {
              val scale    = width / box.getWidth
              val renderer = new PDFRenderer(document)

              val image = renderer.renderImage(i, scale)

              ImageIO.write(
                image,
                "png",
                new java.io.File(filePath.absolute.toString)
              )

              ()
            }
          }
        }
        .compile
        .toList
    }

  private def pdDocument[F[_]: Sync](documentPath: Path): Resource[F, PDDocument] =
    for {
      file     <- Resource.make(openFile(documentPath))(closeFile[F])
      document <- Resource.make(openPdf(file))(closePdf[F])
    } yield document

  private def openPdf[F[_]: Sync](file: RandomAccessFile): F[PDDocument] = Sync[F].blocking {
    val parser = new PDFParser(file)
    parser.parse()
    parser.getPDDocument
  }

  private def closePdf[F[_]: Sync](document: PDDocument): F[Unit] = Sync[F].blocking {
    document.close()
  }

  private def openFile[F[_]: Sync](documentPath: Path): F[RandomAccessFile] = Sync[F].blocking {
    new RandomAccessFile(new java.io.File(documentPath.absolute.toString), "r")
  }

  private def closeFile[F[_]: Sync](file: RandomAccessFile): F[Unit] = Sync[F].blocking {
    file.close()
  }

  trait pdfPage[F[_]] {
    def index: Int
    def aspectRatio: Double
    def renderPng(width: Int, filePath: Path): F[Unit]
  }
}
