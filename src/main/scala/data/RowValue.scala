package data

case class RowValue[F: Fractional](value: F, columnIndex: Int) {
  require(columnIndex >= 0)
}
