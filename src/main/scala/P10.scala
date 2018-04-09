package main.scala

object P10 {

  def encode(a: List[Any]):List[(Int, Any)] = a match {
    case Nil => List((0, None))
    case _ => {
      val (packed, next) = a.span(_ == a.head)
      val curTuple = (packed.length, a.head)
      next match {
        case Nil => List(curTuple)
        case _ => curTuple :: encode(next)
      }
    }
  }
}
