package arithmetic

import arithmetic.P32._
import org.scalatest._

class testP32 extends FlatSpec with Matchers {

  "gcd" should "return greatest common divisor of two given integers" in {
    gcd(36, 63) should be(9)
  }
}
