package test.scala

import main.scala.P02._
import org.scalatest._

class testP02 extends FlatSpec with Matchers {

  "penultimate" should "return the second last item of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    penultimate(mockList) should be(4)
  }

  it should "throw NoSuchElementException" in {
    val emptyList = List[Int]()
    assertThrows[NoSuchElementException] {
      penultimate(emptyList)
    }
  }

  "penultimateN" should "return the last N item of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    penultimateN(mockList, 2) should be(4)
  }

  behavior of "penultimateN (when n is larger than the length)"

  it should "throw NoSuchElementException" in {
    val mockList = List(1, 2)
    assertThrows[NoSuchElementException] {
      penultimateN(mockList, 3)
    }
  }

  "penultimateNOpmtized" should "return the last N item of list" in {
    val mockList = List(1, 2, 3, 4, 5)
    penultimateNOptimized(mockList, 2) should be(4)
  }

  behavior of "penultimateNOptimized (when n is larger than the length)"

  it should "throw NoSuchElementException" in {
    val mockList = List(1, 2)
    assertThrows[NoSuchElementException] {
      penultimateNOptimized(mockList, 3)
    }
  }
}
