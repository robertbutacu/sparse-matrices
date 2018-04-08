package functions

import data.{RowValue, SparseMatrix}

trait SparseMatrixOperations[T[_], F] {
  def ***(A: T[F], B: T[F]): T[F]

  def +++(A: T[F], B: T[F]): T[F]

  def ***(A: T[F], b: List[F]): T[F]
}

object SparseMatrixOperations {

  case class RowIterator[F: Fractional](index: Int, values: Iterator[RowValue[F]])



  case class ConcurrentColumnIterator[F: Fractional](first: RowValueWithIndex[F],
                                                     second: RowValueWithIndex[F])

  type RowParser[F] = Iterator[RowIterator[F]]

  def sparseMatrixOperations = new SparseMatrixOperations[SparseMatrix, Double] {
    override def ***(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = ???

    override def +++(A: SparseMatrix[Double], B: SparseMatrix[Double]): SparseMatrix[Double] = {
      val nrOfColumns = Math.max(A.maxByColumn, B.maxByColumn)
      val nrOfRows = Math.max(A.rows.length, B.rows.length)


      def addMatrices(firstMatrix: RowParser[Double], secondMatrix: RowParser[Double],
                      cri: ConcurrentRowIterator[Double],
                      cci: ConcurrentColumnIterator[Double],
                      resultRow: List[RowValue[Double]],
                      resultRows: List[List[RowValue[Double]]]): SparseMatrix[Double] = {
        /*in every case, 3 cases emerge:
          1. both rows still have elements to parse
          2. first does, but second doesnt
          3. second does, but first doesnt
        */
        (cci.first, cci.second) match {
          //same index, have to be added
          case (RowValueWithIndex(ri, RowValue(ci, x)), RowValueWithIndex(rj, RowValue(cj, y))) if ri == rj && ci == cj => {
            val updatedCurrentRow = resultRow :+ RowValue(ci, x + y)

            (cri.first.hasNext, cri.second.hasNext) match {
              case (true, true) => {
                val nextCCI = ConcurrentColumnIterator(cri.first.next, cri.second.next)
                addMatrices(firstMatrix, secondMatrix, cri, nextCCI, updatedCurrentRow, resultRows)
              }
              case (true, false) => {

                val rowsWithFirst = updatedCurrentRow ::: cri.first.map(r => r.value).toList
                //check if both have next rows
                val nextRowFirstMatrix = firstMatrix.next
                val nextRowSecondMatrix = secondMatrix.next

                val nextCRI = ConcurrentRowIterator(nextRowFirstMatrix, nextRowSecondMatrix)
                val nextCCI = ConcurrentColumnIterator(RowValueWithIndex(nextRowFirstMatrix),
                  RowValueWithIndex(nextRowSecondMatrix))

                addMatrices(firstMatrix, secondMatrix, nextCRI, nextCCI, List.empty, resultRows :+ rowsWithFirst)
              }
              case (false, true) => {
                val rowsWithFirst = updatedCurrentRow ::: cri.second.map(r => r.value).toList
                //check if both have next rows
                val nextRowFirstMatrix = firstMatrix.next
                val nextRowSecondMatrix = secondMatrix.next

                val nextCRI = ConcurrentRowIterator(nextRowFirstMatrix, nextRowSecondMatrix)
                val nextCCI = ConcurrentColumnIterator(RowValueWithIndex(nextRowFirstMatrix),
                  RowValueWithIndex(nextRowSecondMatrix))

                addMatrices(firstMatrix, secondMatrix, nextCRI, nextCCI, List.empty, resultRows :+ rowsWithFirst)
              }
              case (false, false) => {
                val nextRowFirstMatrix = firstMatrix.next
                val nextRowSecondMatrix = secondMatrix.next

                val nextCRI = ConcurrentRowIterator(nextRowFirstMatrix, nextRowSecondMatrix)
                val nextCCI = ConcurrentColumnIterator(RowValueWithIndex(nextRowFirstMatrix),
                  RowValueWithIndex(nextRowSecondMatrix))

                addMatrices(firstMatrix, secondMatrix, nextCRI, nextCCI, List.empty, resultRows :+ updatedCurrentRow)
              }
            }
          }
          case (RowValueWithIndex(ri, RowValue(ci, x)), RowValueWithIndex(rj, RowValue(cj, y))) if ri == rj && ci < cj => {
            SparseMatrix(List.empty)
          }
          case (RowValueWithIndex(ri, RowValue(ci, x)), RowValueWithIndex(rj, RowValue(cj, y))) if ri == rj && ci > cj => {
            SparseMatrix(List.empty)
          }
          case (RowValueWithIndex(ri, RowValue(ci, x)), RowValueWithIndex(rj, RowValue(cj, y))) if ri > rj => {
            SparseMatrix(List.empty)
          }
          case (RowValueWithIndex(ri, RowValue(ci, x)), RowValueWithIndex(rj, RowValue(cj, y))) if ri < rj => {
            SparseMatrix(List.empty)
          }
        }
      }

      val firstMatrixIterator = A.rows.map(r => RowIterator(r.index, r.values.toIterator)).toIterator
      val secondMatrixIterator = B.rows.map(r => RowIterator(r.index, r.values.toIterator)).toIterator

      //the idea would be the following:
      /*
      have a function with the parameters:
      1. firstMatrixIterator
      2. secondMatrixIterator
      3. firstMatrixIterator head buffer
      4. secondMatrixIterator head buffer
      5. curr row
      6. curr column

      The idea would be the following:
      1. pull a new row when the curr column has run out of elements
      2. work with concurrentColumnIterator => compare that with currRow and currColumn
        => whenever the elements match, add them to the result, and pull a new value from the concurrentRowIterator
      3. now, when the column is done, next row => means replace concurrentRowIterator => FOR BOTH VALUES
      4. repeat until parsed everything, wrap the result into a matrix
       */
      SparseMatrix(List.empty)
    }

    override def ***(A: SparseMatrix[Double], b: List[Double]): SparseMatrix[Double] = ???
  }
}