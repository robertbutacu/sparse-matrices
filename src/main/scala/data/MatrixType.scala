package data

trait MatrixType

case object SparseMatrixType extends MatrixType
case object GaussSeidelMatrixType extends MatrixType

object MatrixType {
  def isValid[F: Fractional](matrix: SparseMatrix[F]): Boolean = {
    matrix.matrixType match {
      case SparseMatrixType => true
      case GaussSeidelMatrixType =>
        matrix.rows.forall(r => r.values.exists(v => v.index == r.index))
    }
  }
}
