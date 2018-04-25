package logic

import logic.P49._
import org.scalatest.{FlatSpec, Matchers}

class testP49 extends FlatSpec with Matchers {

  "grayCodeGeneratorNaive" should "return next gray code by given string and generate type" in {
    grayCodeGeneratorNaive("110", 1) should be(Some("111"))
    grayCodeGeneratorNaive("111", 0) should be(Some("101"))
    grayCodeGeneratorNaive("100", 1) should be(None)
    grayCodeGeneratorNaive("100", 0) should be(None)
    grayCodeGeneratorNaive("000", 0) should be(Some("001"))
  }

  "grayNaive" should "return Gray codes in n-bits" in {
    grayNaive(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  "gray" should "return Gray codes in n-bits" in {
    gray(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  "grayMemorization" should "return Gray codes in n-bits" in {
    grayMemorization(3) should be(List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
}
