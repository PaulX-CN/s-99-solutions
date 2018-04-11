package main.scala

import scala.annotation.tailrec

object P17 {

  def split(n: Int, a: List[Any]): (List[Any],List[Any]) = {

    @tailrec
    def _split(n: Int, curList: (List[Any],List[Any])): (List[Any],List[Any]) = (n, curList._2) match {
      case (0, _) => (curList._1.reverse, curList._2)
      case (x, h::t) => _split(x-1, (h :: curList._1, t))
      case (_, Nil) => (curList._1.reverse, curList._2)
    }

    _split(n, (Nil, a))
  }

  def splitFunctional(n: Int, a: List[Any]): (List[Any],List[Any]) = {
    (a.take(n), a.drop(n))
  }

}
