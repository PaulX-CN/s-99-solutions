package lists

import P17._

object P19 {

  def rotate(n: Int, a: List[Any]): List[Any] = {
    if (n >= 0) {
      val (head, tail) = split(n, a)
      tail ::: head
    }
    else {
      val (head, tail) = split(-n, a.reverse)
      (tail ::: head).reverse
    }
  }
}
