package arithmetic

import scala.annotation.tailrec

object P31NP33TOP37NP40 {

  def loop(s: String, i: Int, iter: Iterator[Int]): Unit = {
    // Stop after 200,000
    if (i < 200001) {
      loop(s, iter.next, iter)
    }
  }

  implicit class IntCheckPrime(n: Int) {

    import P32._

    def isPrime: Boolean = (n > 1) && primes.takeWhile(_ <= Math.sqrt(n)).forall { n % _ != 0 }

    // Note filter is applied at the tail part. Otherwise 2.isPrime will cause infinite mutual recursive
    // A important note is using val primes instead of def primes forces Scala to memorize the result of the stream as
    // stream elements.
    private val primes = 2 #:: Stream.from(3, 2).filter { _.isPrime }

    def isCoprimeTo(b: Int): Boolean = gcd(n, b) == 1

    def totient: Int = Stream.from(1).takeWhile(_ <= n).count(_.isCoprimeTo(n))

    def primeFactors: List[Int] = {
      def _primeFactor(current: Int, ps: Stream[Int]): List[Int] = {
        if (current.isPrime) List(current)
        else if (current % ps.head == 0) ps.head :: _primeFactor(current / ps.head, ps)
        else _primeFactor(current, ps.tail)
      }

      _primeFactor(n, primes)
    }

    def primeFactorsMul: Map[Int, Int] = {

      def _countFactors(current: Int, divisor: Int): (Int, Int) = {
        if (current % divisor != 0) (0, current)
        else _countFactors(current / divisor, divisor) match {case (num, cur) => (num + 1, cur)}
      }

      def _primeFactors(current: Int, ps: Stream[Int]): Map[Int, Int] = {
        if (current == 1) Map()
        else if (current.isPrime) Map(current -> 1)
        else if (current % ps.head != 0) _primeFactors(current, ps.tail)
        else {
          val (count, reminder) = _countFactors(current, ps.head)
          Map(ps.head -> count) ++ _primeFactors(reminder, ps)
        }
      }

      _primeFactors(n, primes)
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
        if ((n - p.head).isPrime) (p.head, n - p.head)
        else _primeSum(p.tail)
      }

      _primeSum(primes.takeWhile(_ <= Math.sqrt(n)))
    }

    def goldbachByFind: (Int, Int) = {
      primes.takeWhile(_ <= Math.sqrt(n)).find(p => (n - p).isPrime) match {
        case None    => throw new NoSuchElementException
        case Some(x) => (x, n - x)
      }
    }
  }

}
