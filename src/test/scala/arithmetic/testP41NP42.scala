package arithmetic

import org.scalatest.{FlatSpec, Matchers}
import arithmetic.P41NP42._

class testP41NP42 extends FlatSpec with Matchers {
  
  "GoldbachList" should "return a list of all even numbers and their Goldbach composition " +
    "given a range of integers." in {
    goldbachList(9 to 20) should be(List((3,7), (5, 7), (3, 11), (3, 13), (5, 13), (3, 17)))
  }

  "GoldbachListLimited" should "return a list of all even numbers and their Goldbach composition " +
    "if the 1st number in the composition is larger than limit" +
    "given a range of integers." in {
    goldbachListLimited(1 to 2000, 50) should be(List((73, 919), (61, 1321), (67, 1789), (61, 1867)))
  }
}
