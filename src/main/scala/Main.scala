import data.Simple
import functions.MatrixReader.sparseMatrixReader

object Main extends App {
  val a = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", isWithVector = true, Simple)
  val b = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", isWithVector = true, Simple)
  //val aplusb = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\aplusb.txt", isWithVector = true, AdditionResult)

  //val aplusbResult = SparseMatrixOperations.sparseMatrixOperations.+++(a.matrix, b.matrix)

  a.matrix.asColumns.foreach(println)
}
