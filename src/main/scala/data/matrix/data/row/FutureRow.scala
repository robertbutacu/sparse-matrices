package data.matrix.data.row

import scala.concurrent.Future

case class FutureRow[F: Fractional](index: Int, values: List[Future[RowValue[F]]] = List.empty){
  //require(values.last.columnIndex == index)
}

