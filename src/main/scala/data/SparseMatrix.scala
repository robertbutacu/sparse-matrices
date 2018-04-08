package data

case class SparseMatrix[F: Fractional](rows: List[Row[F]]) {
  require(rows.forall(r => r.values.length <= 10))

  val maxByColumn = rows.maxBy(_.values.length).values.length
}
