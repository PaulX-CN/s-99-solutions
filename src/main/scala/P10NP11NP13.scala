package main.scala

import P09.pack

object P10NP11NP13 {

  def encode(a: List[Any]):List[(Int, Any)] = a match {
    case Nil => Nil
    case _   => {
      val (packed, next) = a.span(_ == a.head)
      val curTuple = (packed.length, a.head)
      curTuple :: encode(next) // if encode(next) is Nil then it will still return List(curTuple)
    }
  }

  def encodeByPack(a: List[Any]):List[(Int, Any)] = {
    pack(a).map(ls => (ls.length, ls.head))
  }

  def encodeModifed(a: List[Any]): List[Any] = {
    pack(a).map(ls => {
      if (ls.length == 1) ls.head
      else (ls.length, ls.head)
    })
  }
}
