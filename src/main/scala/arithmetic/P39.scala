package arithmetic

import P31nP33toP37nP40._

object P39 {

  def listPrimesinRange(r: Range): List[Int] = {
    ((2 #:: Stream.from(3, 2).filter(_.isPrime1)) dropWhile (_ < r.start) takeWhile (_ <= r.end)).toList
  }
}
