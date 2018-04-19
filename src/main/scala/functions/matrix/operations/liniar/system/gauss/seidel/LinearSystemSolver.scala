package functions.matrix.operations.liniar.system.gauss.seidel

import java.util

import data.SparseMatrix
import data.matrix.data.MatrixWithVector
import functions.executor.CurrentTime.printCurrentTime

import scala.annotation.tailrec

object LinearSystemSolver {
  def solve(matrixWithVector: MatrixWithVector[Double], precision: Precision): Option[List[Double]] = {
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
      if (currentIndex == values.length)
        values
      else {
        val updatedValues = (for {
          row <- matrix.rows.zipWithIndex
          if row._1.index == currentIndex
          rowValue <- row._1.values
          if rowValue.index == currentIndex
          valuesWithoutDiagonal = row._1.values.filterNot(_.index == currentIndex)
          valuesWithSolution = valuesWithoutDiagonal.map(p => (p, values(p.index)))
          sum = valuesWithSolution.foldRight(0.0)((curr, acc) => acc + curr._1.value * curr._2)
          vectorValue = vector(currentIndex)
        } yield (( vectorValue - sum ) / rowValue.value, row._2)).head

        val updatedApproximations =
          (values.slice(0, updatedValues._2) :+ updatedValues._1) ::: values.slice(updatedValues._2 + 1, values.length)

        iterate(matrix, vector, updatedApproximations, currentIndex + 1)
      }
    }

    def hasReachedEnd(pi: List[Double], ci: List[Double]): Boolean = {
      pi.zip(ci).forall(p => Math.abs(p._1 - p._2) <= precision.value)
    }

    @tailrec
    def go(matrix: SparseMatrix[Double],
           vector: List[Double],
           currIteration: List[Double],
           k: Int = 1): List[Double] = {
      val nextIteration = iterate(matrix, vector, currIteration)

      println(s"${printCurrentTime()} Curr iteration $currIteration")
      println(s"${printCurrentTime()} Next iteration $nextIteration")

      if (hasReachedEnd(currIteration, nextIteration) || k >= 10000)
        nextIteration //either should be fine, nextIteration should be slightly more precise tho
      else {
        go(matrix, vector, nextIteration, k + 1)
      }
    }

    println(s"${printCurrentTime()} Starting to solve the system.")
    matrixWithVector.vector.map(v => go(matrixWithVector.matrix, v, List.fill(v.length)(0)))
  }
}
