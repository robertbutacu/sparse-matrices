import data.{AdditionResult, MultiplicationResult, Simple, SparseMatrix}
import functions.MatrixReader.sparseMatrixReader
import functions.SparseMatrixOperations.sparseMatrixOperations

object Main extends App {
  val aPath = "E:\\projects\\sparse-matrices\\resources\\a.txt"
  val bPath = "E:\\projects\\sparse-matrices\\resources\\b.txt"
  val aplusbPath = "E:\\projects\\sparse-matrices\\resources\\aplusb.txt"
  val atimesbPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

  val a = sparseMatrixReader.readFromFile(aPath, isWithVector = true, Simple)
  val b = sparseMatrixReader.readFromFile(bPath, isWithVector = true, Simple)
  val aplusbExpected = sparseMatrixReader.readFromFile(aplusbPath, isWithVector = true, AdditionResult)
  val atimesbExpected = sparseMatrixReader.readFromFile(atimesbPath, isWithVector = true, MultiplicationResult)

  //val aplusbActual = sparseMatrixOperations.+++(a.matrix, b.matrix)

 // println(SparseMatrix.equals(aplusbActual, aplusbExpected.matrix))
  //a.matrix.asColumns.foreach(println)

  val atimesbActual = sparseMatrixOperations.***(a.matrix, b.matrix)

  //atimesbActual.rows.foreach(println)
  //atimesbExpected.matrix.rows.foreach(println)
  println(SparseMatrix.equals(atimesbActual, atimesbExpected.matrix))
}
