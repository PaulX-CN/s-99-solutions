package lists

object P27 {

  import P23toP26.combination

  def group3[A](ls: List[A]): List[List[List[A]]] = {
    for {
      a <- combination(2, ls)
      remainList = ls diff a
      b <- combination(3, remainList)
    } yield List(a, b, remainList diff b)
  }

  def groupN[A](n: List[Int], ls: List[A]): List[List[List[A]]] = n match {
    case n :: ns => {
      combination(n, ls) flatMap { firstCombination => // take all the possible first combination
        groupN(ns, ls diff firstCombination) map { firstCombination :: _ } // put in front of subcombination
      }
    }
    case _       => List(Nil)
  }
}
