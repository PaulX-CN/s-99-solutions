package lists

import lists.P22._
import org.scalatest._

class testP22 extends FlatSpec with Matchers {

  "range" should "create a list of intergers from start to end" in {
    range(1, 3) should be(List(1, 2, 3))
  }

  "rangeTailRecursive" should "create a list of intergers from start to end" in {
    rangeTailRecursive(1, 3) should be(List(1, 2, 3))
  }

  "rangeFunctional" should "create a list of intergers from start to end" in {
    rangeFunctional(1, 3) should be(List(1, 2, 3))
  }

}
