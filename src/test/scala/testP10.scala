import main.scala.P10._
import org.scalatest._


class testP10 extends FlatSpec with Matchers {

  "encode" should "pack all same items into subarray" in {
    val mockList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    encode(mockList) should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  "encodeByPack" should "pack all same items into subarray" in {
    val mockList = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    encodeByPack(mockList) should be(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
}