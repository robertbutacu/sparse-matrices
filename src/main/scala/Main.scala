import data.{RowValue, SparseMatrix}
import functions.{MatrixReader, SparseMatrixOperations}

object Main extends App {
  val a = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", isWithVector = true)
  val b = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", isWithVector = true)
  val aplusb = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\aplusb.txt", isWithVector = true)

  val aplusbResult = SparseMatrixOperations.sparseMatrixOperations.+++(a.matrix, b.matrix)

  println(SparseMatrix.equals(aplusb.matrix, aplusbResult))

  println(aplusbResult.rows.last)
  println(aplusb.matrix.rows.last)
}
