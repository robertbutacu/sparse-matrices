package functions.reader

import data._
import data.matrix.data.row.{Row, RowValue}
import data.matrix.data.{MatrixWithVector, row}
import functions.executor.CurrentTime.printCurrentTime
import functions.reader.FileReader._

import scala.io.Source

trait MatrixReader[F] {
  def readFromFile(filename: String, isWithVector: Boolean, rowsType: RowsType,
                   matrixType: MatrixType = SparseMatrixType, isDiagonalElementOnLastPosition: Boolean = false): MatrixWithVector[F]
}

object MatrixReader {
  def sparseMatrixReader: MatrixReader[Double] = (filename: String,
                                                  isWithVector: Boolean,
                                                  rowsType: RowsType,
                                                  matrixType: MatrixType,
                                                  isDiagonalElementOnLastPosition: Boolean) => {
    println(s"${printCurrentTime()} Started reading the matrix")

    val lines = Source.fromFile(filename).getLines()

    val numberOfLines = lines.next().toInt

    lines.drop(1) // dropping the empty line

    println(s"${printCurrentTime()} Reading vector if needed")

    val vector = if (isWithVector) Some(readVector(lines, numberOfLines, List.empty)) else None

    lines.drop(1) // dropping the empty line

    println(s"${printCurrentTime()} Reading matrix")

    val rows = readMatrix(lines)

    println(s"\n${printCurrentTime()} Read matrix")

    val groupedByRow = rows.groupBy(_.rowIndex).values.toList

    val preMappedMatrixRows = groupedByRow.sortBy(r => r.head.rowIndex)

    val mappedMatrixRows = for {
      row <- preMappedMatrixRows
      transformedRow = Row(row.head.rowIndex, row.map(_.value))
      noDoubleElements = addSameElements(transformedRow)
      sorted = Row(noDoubleElements.index, noDoubleElements.values.sortBy(_.index))
    } yield if (isDiagonalElementOnLastPosition) moveDiagonalElement(sorted) else sorted

    println(s"${printCurrentTime()} Finished processing")

    println()

    MatrixWithVector[Double](SparseMatrix(mappedMatrixRows, rowsType, matrixType), vector)
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

    val swappedElements = rowWithIndex.find(_._1.index == transformedRow.index) match {
      case None => values
      case Some(v) =>
        values.slice(0, v._2) ::: values.slice(v._2 + 1, transformedRow.values.length - 2) ::: List(v._1)
    }

    Row(transformedRow.index, swappedElements)
  }

  def addSameElements(transformedRow: Row[Double]): Row[Double] = {
    val noDoubleElements = transformedRow.values.groupBy(_.index).values.map { v =>
      RowValue(v.head.index, v.map(_.value).sum)
    }.toList

    row.Row(transformedRow.index, noDoubleElements)
  }
}
