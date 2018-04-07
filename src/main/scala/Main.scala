import functions.MatrixReader

object Main extends App {
  MatrixReader.sparseMatrixReader.readFromFile[Double]("E:\\projects\\sparse-matrices\\resources\\a.txt")
}
