package main.scala


object P14 {

  def duplicate(a: List[Any]): List[Any] = {
    a.flatMap(e => List.fill(2)(e))
  }
}
