package data

trait MatrixType

case object SparseMatrixType extends MatrixType
case object GaussSeidelMatrixType extends MatrixType

object MatrixType {
  def isValid[F: Fractional](matrix: SparseMatrix[F]): Boolean = {
    def isMatrixTypeRequirementMet: Boolean = matrix.matrixType match {
      case SparseMatrixType => true
      case GaussSeidelMatrixType =>
        matrix.rows.forall(r => r.values.exists(v => v.index == r.index))
    }

    def isMatrixSparse = matrix.rows.forall(r => r.values.length <= RowsType.maximumLength(matrix.rowsType))
    isMatrixSparse && isMatrixTypeRequirementMet
  }
}
