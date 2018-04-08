package functions

import data.{Row, RowValue, RowValueWithIndex, SparseMatrix}

import scala.annotation.tailrec
import scala.io.Source

trait MatrixReader[F] {
  def readFromFile(filename: String, isWithVector: Boolean): MatrixWithVector[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader[Double] = (filename: String, isWithVector: Boolean) => {
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

    val vector = if (isWithVector) Some(readVector(lines, numberOfLines, List.empty)) else None

    lines.drop(1) // dropping the empty line

    val rows = readMatrix(lines, numberOfLines)

    val groupedByRow = rows.groupBy(_.rowIndex).values.toList

    val preMappedMatrixRows = groupedByRow.sortBy(r => r.head.rowIndex)

    val mappedMatrixRows = for {
      row <- preMappedMatrixRows
      transformedRow = Row(row.head.rowIndex, row.map(v => RowValue(v.columnIndex, v.value)))
      noDoubleElements = addSameElements(transformedRow)
    } yield noDoubleElements

    MatrixWithVector[Double](SparseMatrix(mappedMatrixRows), vector)
  }

  def fillWithEmptyRows(rows: List[Row[Double]]): List[Row[Double]] = {
    rows.foldLeft(List.empty[Row[Double]]) { (acc, curr) =>
      if (acc.isEmpty) {
        val emptyRows = (0 until curr.index).map(r => Row[Double](r)).toList
        emptyRows ::: List(curr)
      }
      else {
        val emptyRows = (0 until (curr.index - acc.last.index - 1)).map(r => Row[Double](r + acc.last.index + 1)).toList
        acc ::: emptyRows ::: List(curr)
      }
    }
  }

  def moveDiagonalElement(transformedRow: Row[Double]): Row[Double] = {
    val rowWithIndex = transformedRow.values.zipWithIndex
    val values = transformedRow.values

    val swappedElements = rowWithIndex.find(_._1.columnIndex == transformedRow.index) match {
      case None => values
      case Some(v) =>
        values.slice(0, v._2) ::: values.slice(v._2 + 1, transformedRow.values.length - 2) ::: List(v._1)
    }

    Row(transformedRow.index, swappedElements)
  }

  def addSameElements(transformedRow: Row[Double]): Row[Double] = {
    val noDoubleElements = transformedRow.values.groupBy(_.columnIndex).values.map { v =>
      RowValue(v.head.columnIndex, v.map(_.value).sum)
    }.toList

    Row(transformedRow.index, noDoubleElements)
  }
}
