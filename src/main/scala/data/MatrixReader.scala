package data

trait MatrixReader {
  def readFromFile[F: Fractional](filename: String): SparseMatrix[F]
}

object MatrixReader {

}
