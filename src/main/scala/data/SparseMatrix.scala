package data

case class SparseMatrix[F: Fractional](rows: List[Row[F]], matrixType: MatrixType = Simple) {
  require(rows.forall(r => r.values.length <= MatrixType.maximumLength(matrixType)))

  val maxByColumn: Int = rows.maxBy(_.values.length).values.length


}

object SparseMatrix {
  def equals(A: SparseMatrix[Double], B: SparseMatrix[Double]): Boolean = {
    def areElementsTheSame(f: List[RowValue[Double]], s: List[RowValue[Double]]): Boolean = {
      def round(n: Double) = Math.floor(n * 100) / 100
      f.zip(s).forall(p => p._1.columnIndex == p._2.columnIndex && round(p._1.value) == round(p._2.value))
    }
    A.rows.zip(B.rows).forall(r => r._1.index == r._2.index && areElementsTheSame(r._1.values, r._2.values))
  }
}