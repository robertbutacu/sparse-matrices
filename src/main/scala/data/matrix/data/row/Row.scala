package data.matrix.data.row

case class Row[F: Fractional](index: Int, values: List[RowValue[F]] = List.empty)