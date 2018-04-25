package data.matrix.data

import data.SparseMatrix

case class MatrixWithVector[F: Fractional](matrix: SparseMatrix[F], vector: Option[List[F]] = None) {
  require(vector.forall(v => v.length == matrix.rows.last.index + 1))
}
