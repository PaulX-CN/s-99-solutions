package test.scala

import main.scala.P03._
import org.scalatest._

class testP03 extends FlatSpec with Matchers {

  "nth" should "return the nth item of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    nth(2, mockList) should be(3)
  }

  it should "throw IllegalArgumentException" in {
    val emptyList = List(1, 2)
    assertThrows[IllegalArgumentException] {
      nth(-1, emptyList)
    }
  }

  it should "throw NoSuchElementException" in {
    val emptyList = List[Int]()
    assertThrows[NoSuchElementException] {
      nth(1, emptyList)
    }
  }
}