package data

case class RowValueWithIndex[F: Fractional](value: F, rowIndex: Int, columnIndex: Int) {
  require(columnIndex >= 0)
}
