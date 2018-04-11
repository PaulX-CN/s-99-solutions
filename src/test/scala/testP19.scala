import main.scala.P19._
import org.scalatest._

class testP19 extends FlatSpec with Matchers {

  "rotate" should "rotate the array at n+1th element" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    rotate(3, mockList) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  "rotate" should "do nothing if n is 0" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    rotate(0, mockList) should be(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "rotate" should "rotate the array at (length-n) element if n is negative" in {
    val mockList = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    rotate(-2, mockList) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

}
