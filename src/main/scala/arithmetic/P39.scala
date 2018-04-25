package arithmetic

import P31NP33TOP37NP40._

object P39 {

  def listPrimesinRange(r: Range): List[Int] = {
    Stream.cons(2, Stream.from(3, 2).filter(_.isPrime1)).dropWhile(_ < r.start).takeWhile(_ <= r.end).toList
  }
}
