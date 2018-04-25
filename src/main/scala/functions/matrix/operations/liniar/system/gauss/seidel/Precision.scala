package functions.matrix.operations.liniar.system.gauss.seidel

case class Precision(magnitude: Int) {
  require(magnitude >= 0)

  def value: Double = Math.pow(10, -magnitude)
}
