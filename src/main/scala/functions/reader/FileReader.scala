package functions.reader

import data.matrix.data.row.{RowValue, RowValueWithIndex}

import scala.annotation.tailrec

object FileReader {
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

  def readMatrix(lines: Iterator[String]): List[RowValueWithIndex[Double]] = {
    @tailrec
    def go(lines: Iterator[String],
           matrixLines: List[RowValueWithIndex[Double]]): List[RowValueWithIndex[Double]] = {
      if (!lines.hasNext)
        matrixLines
      else {
        val currLine = lines.take(1)

        val rowValues = currLine.toList.head.split(", ")

        val rowValue = RowValueWithIndex(rowValues(1).toInt, RowValue(rowValues(2).toInt, rowValues(0).toDouble))

        go(lines, matrixLines :+ rowValue)
      }
    }

    go(lines, List.empty)
  }
}
