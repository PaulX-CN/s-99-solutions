package main.scala

import scala.annotation.tailrec

object P18 {

  def slice(start: Int, end:Int, a: List[Any]): List[Any] = {
    a.zipWithIndex filter {v => v._2 >= start && v._2 < end} map {_._1}
  }

  def slice2(start: Int, end:Int, a: List[Any]): List[Any] = {
    a.drop(start) take (end - (start max 0))
  }

  def slice3(start: Int, end:Int, a: List[Any]): List[Any] = {

    @tailrec
    def _slice(c: Int, curList: List[Any], prevList: List[Any]): List[Any] = (c, prevList) match {
      case (_, Nil) => curList.reverse
      case (x, h::t) if x<start => _slice(x+1, curList, t)
      case (x, _) if x>=end => curList.reverse
      case (x, h::t) => _slice(x+1, h :: curList, t)
    }

    _slice(0, Nil, a)
  }
}
