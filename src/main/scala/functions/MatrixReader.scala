package functions

import java.io.File

import data.SparseMatrix

import scala.io.Source

trait MatrixReader {
  def readFromFile[F: Fractional](filename: String): SparseMatrix[F]
}

object MatrixReader {
  implicit def sparseMatrixReader: MatrixReader = new MatrixReader {
    override def readFromFile[F: Fractional](filename: String): SparseMatrix[F] = {
      def readVector(file: File): List[Double] = {
        List.empty
      }

      def readMatrix(file: File): List[(Double, Int)] = {
        List.empty
      }

      val lines = Source.fromFile(filename).getLines()

      for{
        line <- lines
      } println(line)
      SparseMatrix(List.empty)
    }
  }
}
