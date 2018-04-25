package functions.executor

import data.SparseMatrix
import functions.matrix.operations.SparseMatrixOperations._
import functions.executor.CurrentTime.printCurrentTime
import functions.matrix.operations.SparseMatrixOperations

object OperationExecutor {
  def multiplyMatrices(m1: SparseMatrix[Double],
                       m2: SparseMatrix[Double],
                       expectedResult: SparseMatrix[Double]): Unit = {
    println()

    println(s"""${printCurrentTime()} Multiplying matrices """)
    val atimesbActual = sparseMatrixOperations.***(m1, m2)
    println(s"[${printCurrentTime()}] Finished multiplying")

    //println(s"""Checking equality ${printCurrentTime()}""")
    //println(SparseMatrix.equals(atimesbActual, expectedResult))
  }

  def addMatrices(m1: SparseMatrix[Double],
                  m2: SparseMatrix[Double],
                  expectedResult: SparseMatrix[Double]): Unit = {
    println()

    println(s"""${printCurrentTime()} Starting adding matrices""")
    val aplusbActual = sparseMatrixOperations.+++(m1, m2)
    println(s"""${printCurrentTime()} Finished adding""")


    //println("Checking equality")
    //println(SparseMatrix.equals(aplusbActual, expectedResult))
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
    println(SparseMatrix.equals(aTimesVector, vectorA))
  }
}
