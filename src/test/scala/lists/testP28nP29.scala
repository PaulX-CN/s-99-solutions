package lists

import lists.P28NP29._
import org.scalatest._

class testP28nP29 extends FlatSpec with Matchers {

  "lsort" should "sort a list by length of the item" in {
    val mockList = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l),
      List('m, 'n), List('o))
    lsortByQuickSort(mockList) should be(
      List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h),
        List('i, 'j, 'k, 'l)))
  }


  "sortByFreq" should "sort a list by the frequency of length of the sublists appeared in this list" in {
    val mockList = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l),
      List('m, 'n), List('o))
    sortByFreq(mockList) should be(
      List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e),
        List('m, 'n)))
  }

}
