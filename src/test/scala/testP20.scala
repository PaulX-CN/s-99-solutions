import main.scala.P20._
import org.scalatest._

class testP20 extends FlatSpec with Matchers {

  "removeAt" should "remove nth element of the list" in {
    val mockList = List('a, 'b, 'c, 'd)
    removeAt(1, mockList) should be((List('a, 'c, 'd), 'b))
  }

}
