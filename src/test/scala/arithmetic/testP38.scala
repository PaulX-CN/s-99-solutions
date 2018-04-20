package arithmetic

import org.scalatest.{FlatSpec, Matchers}

class testP38 extends FlatSpec with Matchers {

  //run with "testOnly arithmetic.testP38 -- -oD"

  // finished in 90 milliseconds
  "testTotient" should "run test on 3 numbers using totient naive approach" in {
    P38.testTotient should be(List(80, 800, 8000))
  }

  // finished in 9 milliseconds
  "testTotientImproved" should "run test on 3 numbers using totient calculation approach" in {
    P38.testTotientImproved should be(List(80, 800, 8000))
  }
}
