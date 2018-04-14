import data._
import functions.CurrentTime.printCurrentTime
import functions.MatrixReader.sparseMatrixReader
import functions.OperationExecutor._

object Main extends App {
  println(s"""Starting reading resources ${printCurrentTime()}""")
  val aPath = "E:\\projects\\sparse-matrices\\resources\\a.txt"
  val bPath = "E:\\projects\\sparse-matrices\\resources\\b.txt"
  val aplusbPath = "E:\\projects\\sparse-matrices\\resources\\aplusb.txt"
  val atimesbPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

  val a = sparseMatrixReader.readFromFile(aPath, isWithVector = true, Simple)
  val aVector = a.vector.get
  val b = sparseMatrixReader.readFromFile(bPath, isWithVector = true, Simple)
  val bVector = b.vector.get

  //val aplusbExpected = sparseMatrixReader.readFromFile(aplusbPath, isWithVector = true, AdditionResult)
  //val atimesbExpected = sparseMatrixReader.readFromFile(atimesbPath, isWithVector = true, MultiplicationResult)

  //multiplyMatrices(a.matrix, b.matrix, atimesbExpected)
  multiplyMatrices(a.matrix, b.matrix, SparseMatrix(List.empty, Simple))

  //addMatrices(a.matrix, b.matrix, aplusbExpected.matrix)
  addMatrices(a.matrix, b.matrix, SparseMatrix(List.empty, Simple))

  multiplyWithVector(a.matrix, aVector)

  multiplyWithVector(b.matrix, bVector)

  //println(a.matrix.rows.maxBy(_.index))
}
