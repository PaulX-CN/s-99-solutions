package lists

object P14NP15 {

  def duplicate(a: List[Any]): List[Any] = {
    a.flatMap(e => List.fill(2)(e))
  }

  def duplicateN(n: Int, a: List[Any]): List[Any] = {
    a.flatMap(e => List.fill(n)(e))
  }
}
