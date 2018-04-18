package functions.matrix.operations.liniar.system.gauss.seidel

import data.matrix.data.MatrixWithVector

object LinearSystemSolver {
  def solve(matrixWithVector: MatrixWithVector[Double], precision: Precision): List[Double] = {
    /**
      *
      * @param matrixWithVector - ???
      * @param values - current iteration's values
      * @param currentIndex - current iteration's index of the value
      * @return - the next generation of values
      */
    def iterate(matrixWithVector: MatrixWithVector[Double],
                values: List[Double],
                currentIndex: Int = 0): List[Double] = {
      List.empty
    }

    List.empty
  }
}
