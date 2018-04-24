import data.SparseMatrix
import data.matrix.data.MatrixWithVector
import data.matrix.data.row.{Row, RowValue}
import functions.labs.LabRunner
import functions.matrix.operations.SparseMatrixOperations._
import functions.matrix.operations.liniar.system.gauss.seidel.{LinearSystemSolver, Precision}

object Main extends App {
  //LabRunner.runLab4()
  val matrix = SparseMatrix(List(
    Row(0, List(RowValue(0, 4.0), RowValue(1, -1.0), RowValue(2, -1.0))),
    Row(1, List(RowValue(0, -2.0), RowValue(1, 6.0), RowValue(2, 1.0))),
    Row(2, List(RowValue(0, -1.0), RowValue(1, 1.0), RowValue(2, 7.0)))
  ))
  //LabRunner.runLab5()
  //LinearSystemSolver.solve(MatrixWithVector(matrix, Some(List(3.0, 9.0, -6.0))), Precision(3))

  println(sparseMatrixOperations.applyOperation(matrix, matrix, implicitly[Fractional[Double]].minus))

}
