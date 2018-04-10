package data.matrix.data

trait MatrixElement[F] {
  def index: Int
  def value: F
}
