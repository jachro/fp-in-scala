package tests

object Barclays {

  def sumUpColumns(matrix: Array[Array[Int]]): Array[Int] =
    matrix.foldLeft(Array.fill(matrix(0).length)(0))(
      (result, row) => {
        row.zipWithIndex.foreach {
          case (matrixItem, idx) =>
            val columnSum = result(idx) + matrixItem
            result.update(idx, columnSum)
        }
        result
      }
    )

  def sumUpColumnsWithList(matrix: Array[Array[Int]]): List[Int] =
    matrix.foldLeft(List.empty[Int])(
      (result, row) =>
        row.zipAll(result, 0, 0).foldLeft(List.empty[Int]) {
          case (newResult, (rowItem, oldResultItem)) =>
            newResult :+ rowItem + oldResultItem
        }
    )

}
