package main.scala

import scala.annotation.tailrec

object P22 {

  def range(start: Int, end: Int): List[Int] = {
    if (start == end) List(start)
    else start :: range(start + 1, end)
  }

  def rangeTailRecursive(start: Int, end: Int): List[Int] = {
    @tailrec
    def _range(curList: List[Int], s: Int, e: Int): List[Int] = {
      if (s > e) curList.reverse
      else _range(s :: curList, s + 1, e)
    }

    _range(Nil, start, end)
  }

  def unfoldRight[A, B](start: B)(f: B => Option[(A, B)]): List[A] = f(start) match {
    case None         => Nil
    case Some((h, r)) => h :: unfoldRight(r)(f)
  }

  def rangeFunctional(start: Int, end: Int): List[Int] = {
    unfoldRight(start) { s =>
      if (s > end) None
      else Some((s, s + 1))
    }
  }
}
