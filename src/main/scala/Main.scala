import data.SparseMatrix
import data.matrix.data.MatrixWithVector
import data.matrix.data.row.{Row, RowValue}
import functions.labs.LabRunner
import functions.matrix.operations.liniar.system.gauss.seidel.{LinearSystemSolver, Precision}

object Main extends App {
  //LabRunner.runLab4()
  val matrix = SparseMatrix(List(
    Row(0, List(RowValue(0, 102.5))),
    Row(1, List(RowValue(0, 3.5), RowValue(1, 104.88), RowValue(2, 1.05), RowValue(4, 0.33))),
    Row(2, List(RowValue(2, 100.0))),
    Row(3, List(RowValue(1, 1.3), RowValue(3, 101.3))),
    Row(4, List(RowValue(0, 0.73), RowValue(3, 1.5), RowValue(4, 102.23)))
  ))
  //LabRunner.runLab5()
  LinearSystemSolver.solve(MatrixWithVector(matrix, Some(List(6.0, 7.0, 8.0, 9.0, 1.0))), Precision(5))
}
