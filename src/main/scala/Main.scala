import functions.MatrixReader

object Main extends App {
  val a = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", true)
  val b = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", true)
}
