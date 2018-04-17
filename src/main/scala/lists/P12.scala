package lists

object P12 {

  def decode(a: List[(Int, Symbol)]): List[Symbol] = {
    (List[Symbol]() /: a) ((previous, cur) => previous ::: List.fill(cur._1)(cur._2))
  }

  def decodeByFlatmap(a: List[(Int, Symbol)]): List[Symbol] = {
    a.flatMap(e => List.fill(e._1)(e._2))
  }
}
