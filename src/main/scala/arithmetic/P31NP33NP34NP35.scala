package arithmetic

import scala.annotation.tailrec

object P31NP33NP34NP35 {

  implicit class IntCheckPrime(n: Int) {

    import P32._

    def isPrime: Boolean = {
      (n > 1) && primes.takeWhile(_ <= Math.sqrt(n)).forall {n % _ != 0}
    }

    // Note filter is applied at the tail part. Otherwise 2.isPrime will cause infinite mutual recursive
    private val primes = Stream.cons(2, Stream.from(3, 2).filter {_.isPrime})

    def isCoprimeTo(b: Int): Boolean = gcd(n, b) == 1


    def totient: Int = {
      Stream.from(1).takeWhile(_ <= n).count(_.isCoprimeTo(n))
    }

    def primeFactors: List[Int] = {
      def _primeFactor(current: Int, ps: Stream[Int]): List[Int] = {
        if (current.isPrime) List(current)
        else if (current % ps.head == 0) ps.head :: _primeFactor(current / ps.head, ps)
        else _primeFactor(current, ps.tail)
      }

      _primeFactor(n, primes)
    }
  }
}
