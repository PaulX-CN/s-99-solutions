package arithmetic

import arithmetic.P31NP33NP34NP35._
import org.scalatest._

class testP31NP33NP34NP35 extends FlatSpec with Matchers {

  "IntCheckPrime" should "implicitly convert integer to IntCheckPrime class" in {
    2.isPrime should be(true)
  }

  "isCoPrime" should "return true if two integers' greatest common divisor equals 1" in {
    35.isCoprimeTo(64) should be(true)
  }

  "totient" should "return the number of positive integers r (1 <= r <= m) that are coprime to m." in {
    10.totient should be(4)
    100.totient should be(40)
  }

  "primeFactors" should "return a flat list containing the prime factors in ascending order" in {
    315.primeFactors should be(List(3, 3, 5, 7))
  }

}
