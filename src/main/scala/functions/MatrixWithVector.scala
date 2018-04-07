package functions

import data.SparseMatrix

case class MatrixWithVector[F: Fractional](matrix: SparseMatrix[F], vector: Option[List[F]] = None)
