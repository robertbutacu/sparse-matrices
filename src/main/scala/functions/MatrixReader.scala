package functions

import java.io.File

import data.{RowValue, SparseMatrix}

import scala.annotation.tailrec
import scala.io.Source

trait MatrixReader {
  def readFromFile[F: Fractional](filename: String): SparseMatrix[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader = new MatrixReader {
    override def readFromFile[F: Fractional](filename: String): SparseMatrix[F] = {
      def readVector(lines: Iterator[String]): List[Double] = {
        List.empty
      }

      def readMatrix(lines: Iterator[String], toParse: Int): List[RowValue[F]] = {
        @tailrec
        def go(lines: Iterator[String], toParse: Int, matrixLines: List[RowValue[F]]): List[RowValue[F]] = {
          if (toParse == 0)
            matrixLines
          else {
            val currLine = lines.take(1)

            go(lines, toParse - 1, matrixLines)
          }
        }

        go(lines, toParse, List.empty)
      }

      val lines = Source.fromFile(filename).getLines()

      val numberOfLines = lines.take(1)

      println(numberOfLines.toList.head.toInt)
      SparseMatrix(List.empty)
    }
  }
}
