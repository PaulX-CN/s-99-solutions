package lists

object P04 {
  def lengthOf[A](a: List[A]): Int = {
    (0 /: a) ((c, _) => c + 1)
  }
}
