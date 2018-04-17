package lists

import lists.P05._
import org.scalatest._

class testP05 extends FlatSpec with Matchers {

  "lengthOf" should "return the reverse of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    reverseOf(mockList) should be(List(5, 4, 3, 2, 1))
  }

}
