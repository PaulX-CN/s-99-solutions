import main.scala.P12._
import org.scalatest._


class testP12 extends FlatSpec with Matchers {

  "decode" should "unpack all subarray into flatten array" in {
    val mockList = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    decode(mockList) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

}
