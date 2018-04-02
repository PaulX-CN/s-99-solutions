package main.scala


object P01 {

  def last[A](a: List[A]): A = {
    a match {
      case h :: Nil => h
      case _ :: t   => last(t)
      case _        => throw new NoSuchElementException
    }
  }
}
