package data

case class Row[F: Fractional](index: Int, values: List[RowValue[F]])
