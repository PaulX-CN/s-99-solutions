package binaryTree

import binaryTree.P60._
import org.scalatest.{FlatSpec, Matchers}

class testP60 extends FlatSpec with Matchers {

  "hbalTreesWithNodes" should "construct all the height-balanced " +
    "                         binary trees with a given nuber of nodes." in {
    hbalTreesWithNodes(4, "x").map(_.toString) should contain allOf(
      "T(x T(x T(x . .) .) T(x . .))", "T(x T(x . T(x . .)) T(x . .))"
    )
  }

  "maxHbalHeight" should "return maximum height with given node numbers" in {
    maxHbalHeight(4) should be(3)
  }

  "minHbalHeight" should "return minimum height with given node numbers" in {
    minHbalHeight(4) should be(3)
  }
}
