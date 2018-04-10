package data.matrix.data.row

case class RowValue[F: Fractional](columnIndex: Int, value: F)
