package test.scala

import main.scala.P01._
import org.scalatest._

class testP01 extends FlatSpec with Matchers {

  "last" should "get last value of list" in {
    val mockList: List[Int] = List(1, 2, 3, 4)
    last(mockList) should be(4)
  }

  it should "throw NoSuchElementException if list is empty" in {
    val emptyList = List[Int]()
    a[NoSuchElementException] should be thrownBy {
      last(emptyList)
    }
  }
}