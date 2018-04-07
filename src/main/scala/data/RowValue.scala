package data

case class RowValue[F: Fractional](value: F, rowIndex: Int, columnIndex: Int) {
  require(columnIndex >= 0)
}
