import main.scala.P18._
import org.scalatest._


class testP18 extends FlatSpec with Matchers {

  "slice" should "split list at nth element" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    slice(3, 7, mockList) should be(List('d, 'e, 'f, 'g))
  }

  "slice3" should "split list at nth element" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    slice3(3, 7, mockList) should be(List('d, 'e, 'f, 'g))
  }

}
