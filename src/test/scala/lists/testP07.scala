package lists

import lists.P07._
import org.scalatest._


class testP07 extends FlatSpec with Matchers {

  "flatten" should "return flattened list" in {
    val mockList = List(1, List(2), List(3, List(4)))
    flatten(mockList) should be(List(1, 2, 3, 4))
  }

  it should "return empty if list is empty" in {
    val emptyList = List()
    flatten(emptyList) should be(List())
  }
}
