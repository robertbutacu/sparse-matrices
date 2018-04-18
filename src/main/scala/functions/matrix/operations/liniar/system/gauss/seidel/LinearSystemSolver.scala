package functions.matrix.operations.liniar.system.gauss.seidel

import data.matrix.data.MatrixWithVector

import scala.annotation.tailrec

object LinearSystemSolver {
  def solve(matrixWithVector: MatrixWithVector[Double], precision: Precision): List[Double] = {

    /**
      *
      * @param matrixWithVector - ???
      * @param values - current iteration's values
      * @param currentIndex - current iteration's index of the value
      * @return - the next generation of values
      */
    @tailrec
    def iterate(matrixWithVector: MatrixWithVector[Double],
                values: List[Double],
                currentIndex: Int = 0): List[Double] = {
      if(currentIndex == values.length)
        values
      else {
        iterate(matrixWithVector, values, currentIndex + 1)
      }
    }

    def hasReachedEnd(pi: List[Double], ci: List[Double]): Boolean = {
      pi.zip(ci).forall(p => Math.abs(p._1 - p._2) <= precision.value )
    }

    List.empty
  }
}
