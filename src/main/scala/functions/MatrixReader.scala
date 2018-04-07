package functions

import java.io.File

import data.{RowValue, SparseMatrix}

import scala.annotation.tailrec
import scala.io.Source

trait MatrixReader[F] {
  def readFromFile(filename: String): SparseMatrix[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader[Double] = (filename: String) => {
    def readVector(lines: Iterator[String]): List[Double] = {
      List.empty
    }

    def readMatrix(lines: Iterator[String], toParse: Int): List[RowValue[Double]] = {
      @tailrec
      def go(lines: Iterator[String], toParse: Int, matrixLines: List[RowValue[Double]]): List[RowValue[Double]] = {
        if (toParse == 0)
          matrixLines
        else {
          val currLine = lines.take(1)

          val rowValues = currLine.toList.head.split(",").map(_.toDouble)

          val rowValue = RowValue(rowValues(0), rowValues(1).toInt, rowValues(2).toInt)

          go(lines, toParse - 1, matrixLines :+ rowValue)
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
