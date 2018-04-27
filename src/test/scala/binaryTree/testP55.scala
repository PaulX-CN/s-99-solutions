package binaryTree

import binaryTree.P55._
import org.scalatest.{FlatSpec, Matchers}

class testP55 extends FlatSpec with Matchers {

  "cBalance" should "return list of all possible complete balanced tree for a given number of nodes" in {
    Tree.cBalance(4, "x") should contain(
      Branch("x",
        Branch("x", Leaf, Leaf),
        Branch("x",
          Branch("x", Leaf, Leaf),
          Leaf))
    )
  }
}
