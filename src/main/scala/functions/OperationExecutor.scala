package functions

import data.SparseMatrix
import functions.CurrentTime.printCurrentTime
import functions.SparseMatrixOperations.sparseMatrixOperations

object OperationExecutor {
  def multiplyMatrices(m1: SparseMatrix[Double],
                       m2: SparseMatrix[Double],
                       expectedResult: SparseMatrix[Double]): Unit = {
    println("\n")

    println(s"""Multiplying matrices ${printCurrentTime()}""")
    val atimesbActual = sparseMatrixOperations.***(m1, m2)
    println(s"Finished multiplying ${printCurrentTime()}")

    //println(s"""Checking equality ${printCurrentTime()}""")
    //println(SparseMatrix.equals(atimesbActual, expectedResult))
  }

  def addMatrices(m1: SparseMatrix[Double],
                  m2: SparseMatrix[Double],
                  expectedResult: SparseMatrix[Double]): Unit = {
    println("\n")

    println(s"""Starting adding matrices ${printCurrentTime()}""")
    val aplusbActual = sparseMatrixOperations.+++(m1, m2)
    println(s"""Finished adding ${printCurrentTime()}""")


    //println("Checking equality")
    //println(SparseMatrix.equals(aplusbActual, expectedResult))
  }

  def multiplyWithVector(m1: SparseMatrix[Double],
                         expectedResult: List[Double]): Unit = {

    println("\n")

    println(s"Started multiplying matrix with vector ${printCurrentTime()}")
    val aVector = (m1.rows.maxBy(_.index).index to 0 by -1).toList.map(_.toDouble)
    val aTimesVector = sparseMatrixOperations.***(m1, aVector)
    println(s"Finished multiplying ${printCurrentTime()}")

    println("Checking equality")
    val vectorA = SparseMatrixOperations.normalizeToSparseMatrix(expectedResult)
    println(SparseMatrix.equals(aTimesVector, vectorA))
  }
}
