import main.scala.P09._
import org.scalatest._


class testP09 extends FlatSpec with Matchers {

  "pack" should "pack all same items into subarray" in {
    val mockList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    pack(mockList) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }
}