package data

trait MatrixType

case object Simple extends MatrixType
case object AdditionResult extends MatrixType
case object MultiplicationResult extends MatrixType
case object VectorType extends MatrixType

object MatrixType {
  def maximumLength(matrixType: MatrixType): Int = {
    matrixType match {
      case Simple => 10
      case AdditionResult => 20
      case MultiplicationResult => Int.MaxValue
      case VectorType => Int.MaxValue
    }
  }
}