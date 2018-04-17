package lists

import lists.P06._
import org.scalatest._

class testP06 extends FlatSpec with Matchers {

  "isPalindromeRecursive" should "return true if list is palindrome" in {
    val mockList = List(1, 2, 3, 3, 2, 1)
    isPalindromeRecursive(mockList) should be(true)
  }


  "isPalindrome" should "return false if list is not palindrome" in {
    val mockList = List(1, 2, 3)
    isPalindromeRecursive(mockList) should be(false)
  }


  "isPalindrome" should "return true if list is empty" in {
    val mockList = List()
    isPalindromeRecursive(mockList) should be(true)
  }
}
