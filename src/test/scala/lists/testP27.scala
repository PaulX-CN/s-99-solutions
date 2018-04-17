package lists

import lists.P27._
import org.scalatest._

class testP27 extends FlatSpec with Matchers {

  "group3" should "group a list of length 9 into sublists of length in 2,3,4" in {
    val mockList = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    group3(mockList) should contain(
      List(List("Aldo", "Beat"), List("Carla", "David", "Evi"), List("Flip", "Gary", "Hugo", "Ida")))
  }

  "groupN" should "group a list into sublists of given length in sequence" in {
    val mockList = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    groupN(List(2, 2, 5), mockList) should contain(
      List(List("Aldo", "Beat"), List("Carla", "David"), List("Evi", "Flip", "Gary", "Hugo", "Ida")))
  }
}
