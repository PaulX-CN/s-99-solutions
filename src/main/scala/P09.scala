package main.scala

object P09 {

  def pack(a: List[Any]):List[List[Any]] = a match {
    case Nil => List(Nil)
    case _ => {
      // method span breaks a list to prefix and postfix by the predicate
      // More list methods check:
      // https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook
      val (packed, next) = a.span(_ == a.head)
      next match {
        case Nil => List(packed)
        case _ => packed :: pack(next)
      }
    }
  }
}
