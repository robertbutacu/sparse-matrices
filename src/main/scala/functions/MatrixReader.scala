package functions

import data.{Row, RowValue, RowValueWithIndex, SparseMatrix}

import scala.annotation.tailrec
import scala.io.Source

trait MatrixReader[F] {
  def readFromFile(filename: String): SparseMatrix[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader[Double] = (filename: String) => {
    @tailrec
    def readVector(lines: Iterator[String],
                   toParse: Int,
                   vector: List[Double]): List[Double] = {
      if (toParse == 0)
        vector
      else {
        val currLine = lines.take(1).toList.head

        readVector(lines, toParse - 1, vector :+ currLine.toDouble)
      }
    }

    def readMatrix(lines: Iterator[String], toParse: Int): List[RowValueWithIndex[Double]] = {
      @tailrec
      def go(lines: Iterator[String],
             toParse: Int,
             matrixLines: List[RowValueWithIndex[Double]]): List[RowValueWithIndex[Double]] = {
        if (toParse == 0)
          matrixLines
        else {
          val currLine = lines.take(1)

          val rowValues = currLine.toList.head.split(", ")

          val rowValue = RowValueWithIndex(rowValues(0).toDouble, rowValues(1).toInt, rowValues(2).toInt)

          go(lines, toParse - 1, matrixLines :+ rowValue)
        }
      }

      go(lines, toParse, List.empty)
    }

    val lines = Source.fromFile(filename).getLines()

    val numberOfLines = lines.take(1).toList.head.toInt

    lines.drop(1) // dropping the empty line

    val vector = readVector(lines, numberOfLines, List.empty)

    lines.drop(1) // dropping the empty line

    val rows = readMatrix(lines, numberOfLines)

    val groupedByRow = rows.groupBy(_.rowIndex).values.toList

    val preMappedMatrixRows = groupedByRow.sortBy(r => r.head.rowIndex)/*.map{r =>
      RowValue(r.head.rowIndex, r.map(v => RowValue(v.rowIndex, v.value)))*/

    val mappedMatrixRows = for {
      row <- preMappedMatrixRows
      transformedRow = Row(row.head.rowIndex, row.map(v => RowValue(v.columnIndex, v.value)))
    } yield transformedRow



    SparseMatrix(List.empty)
  }
}
