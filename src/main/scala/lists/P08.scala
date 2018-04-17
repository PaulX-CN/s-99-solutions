package lists

object P08 {

  def compress(a: List[Any]): List[Any] = {
    def _compress(previous: Any, newList: List[Any], curList: List[Any]): List[Any] = curList match {
      case head :: tail if head == previous => _compress(previous, newList, tail)
      case head :: tail                     => _compress(head, head :: newList, tail)
      case Nil                              => newList.reverse
    }

    _compress(None, Nil, a)
  }

  def compressWithDrop(a: List[Any]): List[Any] = a match {
    case head :: tail => head :: compressWithDrop(tail.dropWhile(_ == head))
    case Nil          => Nil
  }

  def compressWithDropTailRecursive(a: List[Any]): List[Any] = {
    def _compress(newList: List[Any], curList: List[Any]): List[Any] = curList match {
      case head :: tail => _compress(head :: newList, tail.dropWhile(_ == head))
      case Nil          => newList.reverse
    }

    _compress(Nil, a)
  }

  def compressFunctional(a: List[Any]): List[Any] = {
    (a :\ List[Any]()) { (income, previous) =>
      if (previous.isEmpty || income != previous.head) income :: previous
      else previous
    }
  }
}
