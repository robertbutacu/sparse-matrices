package functions

import data.SparseMatrix

trait MatrixReader {
  def readFromFile[F: Fractional](filename: String): SparseMatrix[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader = new MatrixReader {
    override def readFromFile[F: Fractional](filename: String): SparseMatrix[F] = {
      SparseMatrix(List.empty)
    }
  }
}
