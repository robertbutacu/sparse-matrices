package data

case class ColumnValue[F: Fractional](columnIndex: Int, value: F)
