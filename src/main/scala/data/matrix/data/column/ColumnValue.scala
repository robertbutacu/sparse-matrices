package data.matrix.data.column

import data.matrix.data.MatrixElement

case class ColumnValue[F: Fractional](index: Int, value: F) extends MatrixElement[F]
