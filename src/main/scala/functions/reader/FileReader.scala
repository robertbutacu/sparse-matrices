package functions.reader

import data.matrix.data.row.{RowValue, RowValueWithIndex}
import functions.executor.CurrentTime.printCurrentTime

import scala.annotation.tailrec

object FileReader {
  @tailrec
  def readVector(lines: Iterator[String],
                 toParse: Int,
                 vector: List[Double]): List[Double] = {
    if (toParse == 0)
      vector
    else {
      val currLine = lines.next()

      readVector(lines, toParse - 1, vector :+ currLine.toDouble)
    }
  }

  def readMatrix(lines: Iterator[String]): List[RowValueWithIndex[Double]] = {
    @tailrec
    def go(lines: Iterator[String],
           matrixLines: List[RowValueWithIndex[Double]],
           currentIteration: Int = 0): List[RowValueWithIndex[Double]] = {
      if (!lines.hasNext)
        matrixLines
      else {
        val currLine = lines.next()

        val rowValues = currLine.split(", ")

        val rowValue = RowValueWithIndex(rowValues(1).toInt, RowValue(rowValues(2).toInt, rowValues(0).toDouble))

        if (currentIteration % 10000 == 0 && currentIteration > 0) {
          print("|")
          go(lines, matrixLines :+ rowValue, currentIteration + 1)
        }
        else go(lines, matrixLines :+ rowValue, currentIteration + 1)
      }
    }

    println(s"${printCurrentTime()} One | means 10k lines have been read.")
    go(lines, List.empty)
  }
}
