package data.matrix.data.row

import data.matrix.data.MatrixElement

case class RowValue[F: Fractional](index: Int, value: F) extends MatrixElement[F]
