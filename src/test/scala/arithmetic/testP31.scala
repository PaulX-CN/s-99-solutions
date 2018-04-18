package arithmetic

import arithmetic.P31._
import org.scalatest._

class testP31 extends FlatSpec with Matchers {

  "IntCheckPrime" should "implicitly convert integer to IntCheckPrime class" in {
    7.checkPrime should be(true)
  }

}
