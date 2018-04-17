package lists

import lists.P16._
import org.scalatest._


class testP16 extends FlatSpec with Matchers {

  "dropNth" should "drop every nth item in a list" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    dropNth(3, mockList) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "dropNthFunctional" should "drop every nth item in a list" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    dropNthFunctional(3, mockList) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
}
