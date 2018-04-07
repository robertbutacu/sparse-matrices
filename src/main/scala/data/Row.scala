package data

case class Row[F: Fractional](index: Int, values: List[RowValue[F]] = List.empty){
  //require(values.last.columnIndex == index)
}
