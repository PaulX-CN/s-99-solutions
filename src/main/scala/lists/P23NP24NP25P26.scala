package lists

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

  def flatMapSublists[A,B](ls: List[A])(f: List[A] => List[B]): List[B] = ls match {
    case Nil => Nil
    case sublist@(_ :: tail) =>
      f(sublist) ::: flatMapSublists(tail)(f)
  }

  def combination[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n<=0) List(Nil)
    else flatMapSublists(ls){ sublist =>
      combination(n - 1, sublist.tail).map
      { // if sublist does not have any combination available then map will return Nil
        sublist.head :: _
      }
    }
  }
}
