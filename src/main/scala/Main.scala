import functions.MatrixReader

object Main extends App {
  val a = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\a.txt", isWithVector = true)
  val b = MatrixReader.sparseMatrixReader.readFromFile("E:\\projects\\sparse-matrices\\resources\\b.txt", isWithVector = true)

  a.rows.foreach(println)
  println(a.rows.zip((0 to a.rows.length).toList).forall(r => r._1.index == r._2))
}
