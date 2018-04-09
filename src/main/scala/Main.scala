import data.{RowValue, SparseMatrix}
import functions.{MatrixReader, SparseMatrixOperations}

object Main extends App {
  val a = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", isWithVector = true)
  val b = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", isWithVector = true)
  val aplusb = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\aplusb.txt", isWithVector = true)

  val aplusbResult = SparseMatrixOperations.sparseMatrixOperations.+++(a.matrix, b.matrix)

  def areElementsTheSame(f: List[RowValue[Double]], s: List[RowValue[Double]]): Boolean = {
    def round(n: Double) = Math.floor(n * 100) / 100
    f.zip(s).forall(v => v._1.columnIndex == v._2.columnIndex && round(v._1.value) == round(v._2.value))
  }
 // A.rows.zip(B.rows).forall(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  //aplusb.matrix.rows.zip(aplusbResult.rows).dropWhile(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  //  .foreach{r => println(r._1); println(r._2); println()}
  //println(SparseMatrix.equals(aplusb.matrix, aplusbResult))
}
