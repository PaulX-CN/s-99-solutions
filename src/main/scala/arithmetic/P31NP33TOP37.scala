package arithmetic

object P31NP33TOP37 {

  implicit class IntCheckPrime(n: Int) {

    import P32._

    def isPrime: Boolean = (n > 1) && primes.takeWhile(_ <= Math.sqrt(n)).forall { n % _ != 0 }

    // Note filter is applied at the tail part. Otherwise 2.isPrime will cause infinite mutual recursive
    private val primes = Stream.cons(2, Stream.from(3, 2).filter { _.isPrime })

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
  }

}
