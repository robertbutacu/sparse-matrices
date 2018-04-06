package data

case class Row[F: Fractional](index: Int, values: List[RowValue[F]]){
  require(values.last.columnIndex == index)
}
