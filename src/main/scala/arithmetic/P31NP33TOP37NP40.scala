package arithmetic

import scala.annotation.tailrec

object P31NP33TOP37NP40 {


  implicit class IntExtension(n: Int) {

    import P32._

    def isPrime1: Boolean = (n > 1) && primes1.takeWhile(_ <= Math.sqrt(n)).forall {n % _ != 0 }

    // Note filter is applied at the tail part. Otherwise 2.isPrime will cause infinite mutual recursive
    // A important note is using val primes1 instead of def primes1 forces Scala to memorize the result of the stream as
    // stream elements.
    private val primes1 = 2 #:: Stream.from(3, 2).filter { _.isPrime1 }


    // An implementation using generator. However is about 1.1 ~ 1.2 time slower than isPrime1
    // But is it more cost effective?
    def isPrime2: Boolean = (n > 1) && primes2.takeWhile(_ <= Math.sqrt(n)).forall {n % _ != 0 }

    private val primes2:Stream[Int] = {

      def loop(v: Int): Stream[Int] = {
        if (v == 2 || v.isPrime2) v #:: loop(v + 1)
        else loop(v+1)
      }
      loop(2)
    }

    def isCoprimeTo(b: Int): Boolean = gcd(n, b) == 1

    def totient: Int = Stream.from(1).takeWhile(_ <= n).count(_.isCoprimeTo(n))

    def primeFactors: List[Int] = {
      def _primeFactor(current: Int, ps: Stream[Int]): List[Int] = {
        if (current.isPrime1) List(current)
        else if (current % ps.head == 0) ps.head :: _primeFactor(current / ps.head, ps)
        else _primeFactor(current, ps.tail)
      }

      _primeFactor(n, primes1)
    }

    def primeFactorsMul: Map[Int, Int] = {

      def _countFactors(current: Int, divisor: Int): (Int, Int) = {
        if (current % divisor != 0) (0, current)
        else _countFactors(current / divisor, divisor) match {case (num, cur) => (num + 1, cur)}
      }

      def _primeFactors(current: Int, ps: Stream[Int]): Map[Int, Int] = {
        if (current == 1) Map()
        else if (current.isPrime1) Map(current -> 1)
        else if (current % ps.head != 0) _primeFactors(current, ps.tail)
        else {
          val (count, reminder) = _countFactors(current, ps.head)
          Map(ps.head -> count) ++ _primeFactors(reminder, ps)
        }
      }

      _primeFactors(n, primes1)
    }

    def totientImproved: Int = (1 /: n.primeFactorsMul) {
      (prev, h) =>
        h match {
          case (factor, count) =>
            prev * (factor - 1) * Math.pow(factor, count - 1).toInt
        }
    }

    def goldbach: (Int, Int) = {

      @tailrec
      def _primeSum(p: Stream[Int]): (Int, Int) = {
        if ((n - p.head).isPrime1) (p.head, n - p.head)
        else _primeSum(p.tail)
      }

      _primeSum(primes1.takeWhile(_ <= n/2))
    }

    def goldbachByFind: (Int, Int) = {
      primes1.takeWhile(_ <= n/2).find(p => (n - p).isPrime1) match {
        case None    => throw new NoSuchElementException
        case Some(x) => (x, n - x)
      }
    }
  }

}
