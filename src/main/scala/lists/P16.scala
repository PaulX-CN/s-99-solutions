package lists

object P16 {

  def dropNth(n: Int, a: List[Any]): List[Any] = {

    def _drop(curN: Int, curList: List[Any], originalList: List[Any]): List[Any] = originalList match {
      case Nil => curList
      case _ => if (curN == 1) _drop(n, curList, originalList.tail)
                else _drop(curN-1, originalList.head :: curList, originalList.tail)
    }

    _drop(n, Nil, a).reverse
  }

  def dropNthFunctional(n: Int, a: List[Any]): List[Any] = {
    a.zipWithIndex filter {v => (v._2 + 1) % n != 0} map {_._1}
  }
}
