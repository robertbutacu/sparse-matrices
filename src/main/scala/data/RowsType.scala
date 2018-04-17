package data

trait RowsType

case object Simple extends RowsType
case object AdditionResult extends RowsType
case object MultiplicationResult extends RowsType
case object VectorType extends RowsType

object RowsType {
  def maximumLength(matrixType: RowsType): Int = {
    matrixType match {
      case Simple => 10
      case AdditionResult => 20
      case MultiplicationResult => Int.MaxValue
      case VectorType => Int.MaxValue
    }
  }
}