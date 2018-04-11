package main.scala

object P20 {

  def removeAt(n: Int, a: List[Any]): (List[Any], Any) = {
    def _take(curN: Int, newList: List[Any], curList: List[Any]): (List[Any], Any) = curList match {
      case h :: t if curN == n => (newList.reverse ::: t, h)
      case h :: t              => _take(curN + 1, h :: newList, t)
      case Nil                 => (newList.reverse, None)
    }

    _take(0, Nil, a)
  }
}
