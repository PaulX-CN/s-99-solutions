package lists

object P03 {

  def nth[A](n: Int, a: List[A]): A = a match {
    case h :: _ if n == 0 => h
    case h :: _ if n < 0  => throw new IllegalArgumentException
    case _ :: t           => nth(n - 1, t)
    case _                => throw new NoSuchElementException
  }
}
