package lists

import lists.P08._
import org.scalatest._


class testP08 extends FlatSpec with Matchers {

  "compress" should "Eliminate consecutive duplicates of list elements." in {
    val mockList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    compressFunctional(mockList) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "return empty if list is empty" in {
    val emptyList = List()
    compress(emptyList) should be(List())
  }
}
