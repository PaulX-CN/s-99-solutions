package logic

import logic.P46NP47._
import org.scalatest.{FlatSpec, Matchers}

class testP46NP47 extends FlatSpec with Matchers {

  "table2" should "return truth table of given formula" in {
    table2((a: Boolean, b: Boolean) => a and (a or b.not)).toSet should be(
      Set((true,false,true),
        (false,false,false),
        (true,true,true),
        (false,true,false))
    )
  }
}
