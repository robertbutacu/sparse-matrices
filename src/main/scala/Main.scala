import data.{AdditionResult, Simple, SparseMatrix}
import functions.MatrixReader.sparseMatrixReader
import functions.SparseMatrixOperations.sparseMatrixOperations

object Main extends App {
  val a = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", isWithVector = true, Simple)
  val b = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", isWithVector = true, Simple)
  val aplusbExpected = sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\aplusb.txt", isWithVector = true, AdditionResult)

  val aplusbActual = sparseMatrixOperations.+++(a.matrix, b.matrix)

  println(SparseMatrix.equals(aplusbActual, aplusbExpected.matrix))
  //a.matrix.asColumns.foreach(println)
}
