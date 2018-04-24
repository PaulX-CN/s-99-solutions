package arithmetic

import org.scalatest.{FlatSpec, Matchers}

class testP39 extends FlatSpec with Matchers {

  "listPrimesinRange" should "all prime numbers in that rangein given range" in {
    P39.listPrimesinRange(7 to 31) should be(List(7, 11, 13, 17, 19, 23, 29, 31))
  }
}
