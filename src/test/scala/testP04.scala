package test.scala

import main.scala.P04._
import org.scalatest._

class testP04 extends FlatSpec with Matchers {

  "lengthOf" should "return the length of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    lengthOf(mockList) should be(5)
  }

  "lengthOf" should "return 0" in {
    val emptyList = List[Int]()
    lengthOf(emptyList) should be(0)
  }
}