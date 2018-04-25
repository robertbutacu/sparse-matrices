package functions.matrix.operations.liniar.system.gauss.seidel

import data.SparseMatrix
import data.matrix.data.MatrixWithVector
import data.matrix.data.row.{Row, RowValue}
import functions.executor.CurrentTime.printCurrentTime

import scala.annotation.tailrec

object LinearSystemSolver {
  def solve(matrixWithVector: MatrixWithVector[Double], precision: Precision, maxIterations: Int = 10): Option[List[Double]] = {
    /**
      *
      * @param matrix       - ???
      * @param vector       - ???
      * @param values       - approximate values gotten so far
      * @param currentIndex - current index of the current iteration
      * @return - the next generation of values
      */
    @tailrec
    def iterate(matrix: SparseMatrix[Double],
                vector: List[Double],
                values: List[Double],
                currentIndex: Int = 0): List[Double] = {
      def currentRow: Option[(Row[Double], Int)] = {
        val row = matrix.rows
          .zipWithIndex
          .dropWhile(_._2 < currentIndex).head

        if (row._2 == currentIndex) Some(row)
        else None
      }

      def currentValue(currentRow: Option[(Row[Double], Int)]): Option[RowValue[Double]] = {
        currentRow match {
          case Some(row) =>
            val currValue = row._1.values.dropWhile(_.index < currentIndex).head

            if(currValue.index == currentIndex) Some(currValue)
            else None
          case None => None
        }
      }


      if (currentIndex == values.length)
        values
      else {
        val currRow = currentRow
        val currVal = currentValue(currRow)

        val valuesWithoutCurr = currRow.map(r => r._1.values.filterNot(_.index == currentIndex))
        val valuesWithSolutions = valuesWithoutCurr.map(r => r.map(rv => (rv, values(rv.index))))
        val sum = valuesWithSolutions.map(r => r.foldRight(0.0)((curr, acc) => acc + curr._1.value * curr._2))
        val vectorValue = vector(currentIndex)
        (sum, currVal, currRow) match {
          case (Some(s), Some(v), Some(r)) =>
            val updatedValue = ((vectorValue - s) / v.value, r._2)
            val updatedApproximations =
              (values.slice(0, updatedValue._2) :+ updatedValue._1) ::: values.slice(updatedValue._2 + 1, values.length)

            iterate(matrix, vector, updatedApproximations, currentIndex + 1)
          case (_, _, _) => List.empty
        }

        /*val updatedValues = (for {
          row <- matrix.rows.zipWithIndex
          if row._1.index == currentIndex
          rowValue <- row._1.values
          if rowValue.index == currentIndex
          valuesWithoutDiagonal = row._1.values.filterNot(_.index == currentIndex)
          valuesWithSolution = valuesWithoutDiagonal.map(p => (p, values(p.index)))
          sum = valuesWithSolution.foldRight(0.0)((curr, acc) => acc + curr._1.value * curr._2)
          vectorValue = vector(currentIndex)
        } yield ((vectorValue - sum) / rowValue.value, row._2)).head

        val updatedApproximations =
          (values.slice(0, updatedValues._2) :+ updatedValues._1) ::: values.slice(updatedValues._2 + 1, values.length)

        iterate(matrix, vector, updatedApproximations, currentIndex + 1)*/
      }
    }

    def hasReachedEnd(pi: List[Double], ci: List[Double]): Boolean = {
      pi.zip(ci).forall(p => Math.abs(p._1 - p._2) <= precision.value)
    }

    @tailrec
    def go(matrix: SparseMatrix[Double],
           vector: List[Double],
           currIteration: List[Double],
           k: Int = 1,
           previousIterations: List[List[Double]]): Option[List[Double]] = {
      val nextIteration = iterate(matrix, vector, currIteration)

      println(s"${printCurrentTime()} Curr iteration $k: $currIteration")
      println(s"${printCurrentTime()} Next iteration ${k + 1}: $nextIteration\n")

      if (hasReachedEnd(currIteration, nextIteration) || k >= maxIterations)
        if(k >= maxIterations) None
        else Some(nextIteration) //either should be fine, nextIteration should be slightly more precise tho
      else {
        if(previousIterations.exists(pi => pi.zip(nextIteration).forall(p => Math.abs(p._1 - p._2) <= precision.value)))
          None
        else {
          if(previousIterations.length == 5)
            go(matrix, vector, nextIteration, k + 1, previousIterations.slice(0, 3) :+ nextIteration)
          else
            go(matrix, vector, nextIteration, k + 1, previousIterations :+ nextIteration)
        }
      }
    }

    println(s"${printCurrentTime()} Starting to solve the system.\n")
    matrixWithVector.vector match {
      case None => None
      case Some(v) => go(matrixWithVector.matrix, v, List.fill(v.length)(0), 0, List.empty[List[Double]])
    }
  }
}
