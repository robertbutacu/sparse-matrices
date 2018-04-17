package functions.labs

import data.{GaussSeidelMatrixType, Simple, SparseMatrix}
import functions.executor.CurrentTime.printCurrentTime
import functions.executor.OperationExecutor.{addMatrices, multiplyMatrices, multiplyWithVector}
import functions.reader.MatrixReader.sparseMatrixReader

object LabRunner {
  def runLab4(): Unit = {
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

    //addMatrices(a.matrix, b.matrix, aplusbExpected.matrix)
    addMatrices(a.matrix, b.matrix, SparseMatrix(List.empty, Simple))

    multiplyWithVector(a.matrix, aVector)

    multiplyWithVector(b.matrix, bVector)

    //println(a.matrix.rows.maxBy(_.index))
  }

  def runLab5(): Unit = {
    val m1Path = "E:\\projects\\sparse-matrices\\resources\\gauss-seidel\\m1.txt"
    val m2Path = "E:\\projects\\sparse-matrices\\resources\\gauss-seidel\\m2.txt"
    val m3Path = "E:\\projects\\sparse-matrices\\resources\\gauss-seidel\\m3.txt"
    val m4Path = "E:\\projects\\sparse-matrices\\resources\\gauss-seidel\\m4.txt"
    val m5Path = "E:\\projects\\sparse-matrices\\resources\\gauss-seidel\\m5.txt"

    //val m1 = sparseMatrixReader.readFromFile(m1Path, isWithVector = true, Simple, GaussSeidelMatrixType)
    val m2 = sparseMatrixReader.readFromFile(m2Path, isWithVector = true, Simple, GaussSeidelMatrixType)
    //val m3 = sparseMatrixReader.readFromFile(m3Path, isWithVector = true, Simple, GaussSeidelMatrixType)
    //val m4 = sparseMatrixReader.readFromFile(m4Path, isWithVector = true, Simple, GaussSeidelMatrixType)
    //val m5 = sparseMatrixReader.readFromFile(m5Path, isWithVector = true, Simple, GaussSeidelMatrixType)

  }
}