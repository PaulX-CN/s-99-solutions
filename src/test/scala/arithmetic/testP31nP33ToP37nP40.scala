package arithmetic

import arithmetic.P31nP33toP37nP40._
import org.scalatest._

class testP31nP33ToP37nP40 extends FlatSpec with Matchers {

  "isPrime" should "implicitly convert integer to IntExtension class" in {
    2.isPrime1 should be(true)
    2.isPrime2 should be(true)
  }

//  about 1.5 milliseconds
  "testPrime2" should "run the test on isPrime2" in {
    val mockList = (90000 to 100000).toList
    (1 to 3) foreach (x => mockList.map {n => n.isPrime2})
  }

//  about 1.2 milliseconds
  "testPrime1" should "run the test on isPrime1" in {
    val mockList = (90000 to 100000).toList
    (1 to 3) foreach (x => mockList.map {n => n.isPrime1})
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

  "primeFactorsMul" should "return a map with prime factors as key and count of prime factors as value" in {
    315.primeFactorsMul should be(Map(3 -> 2, 5 -> 1, 7 -> 1))
    10.primeFactorsMul should be(Map(2 -> 1, 5 -> 1))
  }

  "totientImproved" should "return the number of positive integers r (1 <= r <= m) that are coprime to m." in {
    10.totientImproved should be(4)
    100.totientImproved should be(40)
  }

  "goldbach" should "return two prime numbers that sum up to a given even integer larger than 2." in {
    12.goldbach should be((5, 7))
    12.goldbachByFind should be((5, 7))
  }
}
