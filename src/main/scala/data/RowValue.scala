package data

case class RowValue[F: Fractional](columnIndex: Int, value: F)
