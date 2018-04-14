import java.util.Calendar

import data.matrix.data.row.{Row, RowValue}
import data._
import functions.MatrixReader.sparseMatrixReader
import functions.SparseMatrixOperations.sparseMatrixOperations
import functions.CurrentTime.printCurrentTime

object Main extends App {


  println(s"""Starting reading resources ${printCurrentTime()}""")
  val aPath = "E:\\projects\\sparse-matrices\\resources\\a.txt"
  val bPath = "E:\\projects\\sparse-matrices\\resources\\b.txt"
  //val aplusbPath = "E:\\projects\\sparse-matrices\\resources\\aplusb.txt"
  //val atimesbPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

  val a = sparseMatrixReader.readFromFile(aPath, isWithVector = true, Simple)
  val aVector = a.vector.get
  val b = sparseMatrixReader.readFromFile(bPath, isWithVector = true, Simple)
  val bVector = b.vector.get

  //val aplusbExpected = sparseMatrixReader.readFromFile(aplusbPath, isWithVector = true, AdditionResult)
  //val atimesbExpected = sparseMatrixReader.readFromFile(atimesbPath, isWithVector = true, MultiplicationResult)

  println("\n")
  println(s"""Starting adding matrices ${printCurrentTime()}""")
  val aplusbActual = sparseMatrixOperations.+++(a.matrix, b.matrix)

  println(s"""Finished adding ${printCurrentTime()}""")
  println("Checking equality")
 // println(SparseMatrix.equals(aplusbActual, aplusbExpected.matrix))

  println("\n")
  println(s"""Multiplying matrices ${printCurrentTime()}""")
  val atimesbActual = sparseMatrixOperations.***(a.matrix, b.matrix)

  println(s"""Finished multiplying ${printCurrentTime()}""")
  //println(s"""Checking equality ${printCurrentTime()}""")
  //println(SparseMatrix.equals(atimesbActual, atimesbExpected.matrix))

  println("\n")
  println(s"Started multiplying matrix with vector ${printCurrentTime()}")
  val atimesVector = sparseMatrixOperations.***(a.matrix, aVector)
  println(s"Finished multiplying ${printCurrentTime()}")

  println("\n")
  println(s"Started multiplying matrix with vector ${printCurrentTime()}")
  val btimesVector = sparseMatrixOperations.***(b.matrix, bVector)
  println(s"Finished multiplying ${printCurrentTime()}")

}
