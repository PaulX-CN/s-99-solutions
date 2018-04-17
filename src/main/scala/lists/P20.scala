package lists

import java.util.NoSuchElementException

object P20 {

  def removeAt[A](n: Int, a: List[A]): (List[A], A) = {
    def _take(curN: Int, newList: List[A], curList: List[A]): (List[A], A) = curList match {
      case _ if n < 0          => throw new NoSuchElementException
      case h :: t if curN == n => (newList.reverse ::: t, h)
      case h :: t              => _take(curN + 1, h :: newList, t)
      case Nil                 => throw new NoSuchElementException
    }

    _take(0, Nil, a)
  }
}
