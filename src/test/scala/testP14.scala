import main.scala.P14._
import org.scalatest._


class testP14 extends FlatSpec with Matchers {

  "duplicate" should "duplicate every item in a list" in {
    val mockList = List('a, 'b, 'c, 'c, 'd)
    duplicate(mockList) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

}
