package lists

object P05 {

  def reverseOf[A](a: List[A]): List[A] = {
    (List[A]() /: a) ((r, h) => h :: r) // appending r to h because it is easier to point r to a new head
  }
}
