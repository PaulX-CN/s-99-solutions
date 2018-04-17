package lists

import scala.annotation.tailrec

object P07 {

  implicit class ListFlatMaps[A](a: List[A]) {

    def flatmap[B](f: A => List[B]): List[B] = a match {
      case head :: tail => f(head) ::: tail.flatmap(f)
      case _            => Nil
    }

    def flatmapTailRecursive[B](f: A => List[B]): List[B] = {

      @tailrec
      def _flatmap(result: List[B])(input: List[A])(f: A => List[B]): List[B] = input match {
        case head :: tail => _flatmap(result ::: f(head))(tail)(f)
        case _            => result
      }

      _flatmap(List[B]())(a)(f)
    }
  }

  def flatten(a: List[Any]): List[Any] = a flatmapTailRecursive {
    case ms: List[_] => flatten(ms)
    case m           => List(m)
  }
}
