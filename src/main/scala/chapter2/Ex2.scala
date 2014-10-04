package chapter2

object Ex2 {

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    array.foldRight[(Boolean, Option[A])]((true, None))((elem, result) => {
      if (!result._1)
        (false, Some(elem))
      else if (result._2.isEmpty)
        (true, Some(elem))
      else
        (ordered(elem, result._2.get), Some(elem))
    })._1
  }

  def isSortedWithLocalFunction[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def areOrdered[A](array: Array[A], ordered: (A, A) => Boolean, idx: Int): Boolean = {
      if (idx + 1 >= array.length)
        true
      else if (ordered(array(idx), array(idx + 1)))
        areOrdered(array, ordered, idx + 1)
      else
        false
    }

    areOrdered(array, ordered, 0)
  }

}
