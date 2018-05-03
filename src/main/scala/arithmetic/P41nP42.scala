package arithmetic

import P31nP33toP37nP40._

object P41nP42 {

  def goldbachList(r: Range): List[(Int, Int)] ={
    goldbachListLimited(r, 0)
  }

  def printGoldbachList(r: Range): Unit = {
    goldbachList(r).foreach(x => {
      val sum = x._1 + x._2
      println(s"$sum = ${x._1} + ${x._2}")
    })
  }

  def goldbachListLimited(r:Range, l: Int): List[(Int, Int)] ={
    (r filter {x => x % 2 == 0 && x > 2} map {n => n.goldbach} filter {x => x._1 > l}).toList
  }
}
