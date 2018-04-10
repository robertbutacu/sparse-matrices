import java.util.Calendar

import data.{AdditionResult, MultiplicationResult, Simple, SparseMatrix}
import functions.MatrixReader.sparseMatrixReader
import functions.SparseMatrixOperations.sparseMatrixOperations

object Main extends App {
  def printCurrentTime(): String = {
    val now = Calendar.getInstance()
    val currentHour = now.get(Calendar.HOUR)
    val currentMinute = now.get(Calendar.MINUTE)
    val currentSecond = now.get(Calendar.SECOND)
    val currentMilli = now.get(Calendar.MILLISECOND)

    s"""$currentHour : $currentMinute : $currentSecond :: $currentMilli"""
  }
  println(s"""Starting reading resources ${printCurrentTime()}""")
  val aPath = "E:\\projects\\sparse-matrices\\resources\\a.txt"
  val bPath = "E:\\projects\\sparse-matrices\\resources\\b.txt"
  val aplusbPath = "E:\\projects\\sparse-matrices\\resources\\aplusb.txt"
  val atimesbPath = "E:\\projects\\sparse-matrices\\resources\\atimesb.txt"

  val a = sparseMatrixReader.readFromFile(aPath, isWithVector = true, Simple)
  val b = sparseMatrixReader.readFromFile(bPath, isWithVector = true, Simple)
  val aplusbExpected = sparseMatrixReader.readFromFile(aplusbPath, isWithVector = true, AdditionResult)
  val atimesbExpected = sparseMatrixReader.readFromFile(atimesbPath, isWithVector = true, MultiplicationResult)

  println("\n\n\n")
  println(s"""Starting adding matrices ${printCurrentTime()}""")
  val aplusbActual = sparseMatrixOperations.+++(a.matrix, b.matrix)

  println(s"""Finished adding ${printCurrentTime()}""")
  println("Checking equality")
  println(SparseMatrix.equals(aplusbActual, aplusbExpected.matrix))

  println("\n\n\n")
  println(s"""Multiplying matrices ${printCurrentTime()}""")
  val atimesbActual = sparseMatrixOperations.***(a.matrix, b.matrix)

  println(s"""Finished multiplying ${printCurrentTime()}""")
  println(s"""Checking equality ${printCurrentTime()}""")
  println(SparseMatrix.equals(atimesbActual, atimesbExpected.matrix))
}
