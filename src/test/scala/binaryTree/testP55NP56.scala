package binaryTree

import binaryTree.P55NP56._
import org.scalatest.{FlatSpec, Matchers}

class testP55NP56 extends FlatSpec with Matchers {

  "cBalance" should "return list of all possible complete balanced tree for a given number of nodes" in {
    Tree.cBalance(4, "x") should contain(
      Branch("x",
        Branch("x", Leaf, Leaf),
        Branch("x",
          Branch("x", Leaf, Leaf),
          Leaf))
    )
  }

  "isSymmetric" should "return true if right subtree is the mirror image of the left subtree" in {
    val mockTree = Branch('a', Branch('b'), Branch('c'))
    mockTree.isSymmetric should be(true)
  }
}
