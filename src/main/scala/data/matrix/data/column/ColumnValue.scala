package data.matrix.data.column

case class ColumnValue[F: Fractional](columnIndex: Int, value: F)
