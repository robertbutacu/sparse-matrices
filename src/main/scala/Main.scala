import java.util.Calendar

import data.matrix.data.row.{Row, RowValue}
import data._
import functions.MatrixReader.sparseMatrixReader
import functions.SparseMatrixOperations.sparseMatrixOperations
import functions.CurrentTime.printCurrentTime
import functions.SparseMatrixOperations

object Main extends App {
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


  println(s"""Starting reading resources ${printCurrentTime()}""")
  val aPath = "E:\\projects\\sparse-matrices\\resources\\a.txt"
  val bPath = "E:\\projects\\sparse-matrices\\resources\\b.txt"
  val aplusbPath = "E:\\projects\\sparse-matrices\\resources\\aplusb.txt"
  val atimesbPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

  val a = sparseMatrixReader.readFromFile(aPath, isWithVector = true, Simple)
  val aVector = a.vector.get
  val b = sparseMatrixReader.readFromFile(bPath, isWithVector = true, Simple)
  val bVector = b.vector.get

  //val aplusbExpected = sparseMatrixReader.readFromFile(aplusbPath, isWithVector = true, AdditionResult)
  //val atimesbExpected = sparseMatrixReader.readFromFile(atimesbPath, isWithVector = true, MultiplicationResult)

  //multiplyMatrices(a.matrix, b.matrix, atimesbExpected)
  multiplyMatrices(a.matrix, b.matrix, SparseMatrix(List.empty, Simple))

  //addMatrices(a.matrix, b.matrix, aplusbExpected)
  addMatrices(a.matrix, b.matrix, SparseMatrix(List.empty, Simple))

  multiplyWithVector(a.matrix, aVector)

  multiplyWithVector(b.matrix, bVector)

  //println(a.matrix.rows.maxBy(_.index))
}
