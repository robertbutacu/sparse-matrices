package data

case class Column[F: Fractional](index: Int, values: List[ColumnValue[F]] = List.empty) {

}
