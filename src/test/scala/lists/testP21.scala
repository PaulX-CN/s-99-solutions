package lists

import lists.P21._
import org.scalatest._


class testP21 extends FlatSpec with Matchers {

  "insertAt" should "insert element at nth of list" in {
    val mockList = List('a, 'b, 'c, 'd)
    insertAt('new, 1, mockList) should be(List('a, 'new, 'b, 'c, 'd))
  }
}
