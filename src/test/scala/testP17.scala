import main.scala.P17._
import org.scalatest._


class testP17 extends FlatSpec with Matchers {

  "split" should "split list at nth element" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    split(3, mockList) should be((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  it should "put all element in the first array if n is larger than the length" in {
    val mockList = List('a, 'b, 'c)
    split(4, mockList) should be((List('a, 'b, 'c), Nil))
  }
}
