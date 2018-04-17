package lists

object P21 {

  def insertAt(x: Any, n: Int, a: List[Any]): List[Any] = a.splitAt(n) match {
    case (pre, post) => pre ::: x :: post // the ::: operation is left associated!!!
  }
}
