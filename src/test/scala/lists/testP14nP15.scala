package lists

import lists.P14nP15._
import org.scalatest._


class testP14nP15 extends FlatSpec with Matchers {

  "duplicate" should "duplicate every item in a list" in {
    val mockList = List('a, 'b, 'c, 'c, 'd)
    duplicate(mockList) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "duplicateN" should "duplicate every item in a list by N time" in {
    val mockList = List('a, 'b, 'c, 'c, 'd)
    val mockTimes = 3
    duplicateN(mockTimes, mockList) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }


}
