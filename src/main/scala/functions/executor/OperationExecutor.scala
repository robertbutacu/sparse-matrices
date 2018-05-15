package functions.executor

import data.{MultiplicationResult, SparseMatrix}
import functions.matrix.operations.SparseMatrixOperations._
import functions.executor.CurrentTime.printCurrentTime
import functions.matrix.operations.SparseMatrixOperations
import functions.matrix.operations.liniar.system.gauss.seidel.Precision
import functions.reader.MatrixReader.sparseMatrixReader

object OperationExecutor {
  def multiplyMatrices(m1: SparseMatrix[Double],
                       m2: SparseMatrix[Double],
                       expectedResult: SparseMatrix[Double]): Unit = {

    lazy val aTimesBPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

    println()

    println(s"${printCurrentTime()} Multiplying matrices")

    val atimesBActual = sparseMatrixOperations.***(m1, m2)

    println(s"[${printCurrentTime()}] Finished multiplying")

    val atimesBExpected = sparseMatrixReader.readFromFile(aTimesBPath,
      isWithVector = true, MultiplicationResult)

    println(SparseMatrix.areEqual(atimesBActual, atimesBExpected.matrix, Precision(6)))
  }

  def addMatrices(m1: SparseMatrix[Double],
                  m2: SparseMatrix[Double],
                  expectedResult: SparseMatrix[Double]): Unit = {
    println()

    println(s"""${printCurrentTime()} Starting adding matrices""")

    val aplusbActual = sparseMatrixOperations.+++(m1, m2)

    println(s"""${printCurrentTime()} Finished adding""")
  }

  def multiplyWithVector(m1: SparseMatrix[Double],
                         expectedResult: List[Double]): Unit = {

    println("\n")

    println(s"${printCurrentTime()} Started multiplying matrix with vector")
    val aVector = (2018 to 0 by -1).toList.map(_.toDouble)
    val aTimesVector = sparseMatrixOperations.***(m1, aVector)

    println(s"${printCurrentTime()} Finished multiplying")

    println("Checking equality")
    val vectorA = SparseMatrixOperations.normalizeToSparseMatrix(expectedResult)
    println(SparseMatrix.areEqual(aTimesVector, vectorA, Precision(3)))
  }
}
