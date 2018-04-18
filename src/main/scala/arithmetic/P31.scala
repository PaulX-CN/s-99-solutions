package arithmetic

object P31 {

  implicit class IntCheckPrime(n: Int) {
    def checkPrime: Boolean = {
      (n > 1) && streams.forall {n % _ != 0}
    }

    val streams: Stream[Int] = {
      Stream.from(2).takeWhile(_ < Math.sqrt(n))
    }
  }
}
