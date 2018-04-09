package data

case class SparseMatrix[F: Fractional](rows: List[Row[F]]) {
  //require(rows.forall(r => r.values.length <= 10))

  val maxByColumn = rows.maxBy(_.values.length).values.length
}

object SparseMatrix {
  def equals(A: SparseMatrix[Double], B: SparseMatrix[Double]): Boolean = {
    def areElementsTheSame(f: List[RowValue[Double]], s: List[RowValue[Double]]): Boolean = {
      def round(n: Double) = Math.floor(n * 100) / 100
      f.forall(r => r.columnIndex == s.head.columnIndex && s.exists(l => round(r.value) == round(l.value)))
    }
    A.rows.zip(B.rows).forall(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  }
}