package main.scala

import scala.util.Random
import P20.removeAt
import P22.rangeFunctional

import scala.annotation.tailrec

object P23NP24NP25P26 {

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    if (n <= 0) Nil
    else {
      val (res, r) = removeAt(Random.nextInt(ls.length), ls)
      r :: randomSelect(n - 1, res)
    }
  }

  def lotto(n: Int, end: Int): List[Int] = {
    randomSelect(n, rangeFunctional(1, end))
  }

  def randomPermute[A](ls: List[A]): List[A] = {

    @tailrec
    def _permute(curList: List[A], prevList: List[A]): List[A] = prevList match {
      case Nil => curList
      case _   => {
        val (res, r) = removeAt(Random.nextInt(prevList.length), prevList)
        _permute(r :: curList, res)
      }
    }

    _permute(Nil, ls)
  }

  def combination[A](n: Int, ls: List[A]): List[List[A]] = {
  }
}
