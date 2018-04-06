package functions

import data.SparseMatrix

trait SparseMatrixOperations {
  def ***[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
  def +++[F: Fractional](A: SparseMatrix[F], B: SparseMatrix[F]): SparseMatrix[F]
}