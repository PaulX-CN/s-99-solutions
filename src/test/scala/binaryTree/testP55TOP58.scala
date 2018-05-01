package binaryTree

import binaryTree.P55TOP58._
import org.scalatest.{FlatSpec, Matchers}

class testP55TOP58 extends FlatSpec with Matchers {

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

  "addValue" should "add an element to a binary search tree" in {
    Leaf.addValue(2) should be(Branch(2))
    Branch(2).addValue(3) should be(Branch(2, Leaf, Branch(3)))
  }

  "fromList" should "construct a binary tree from a list of comparable items" in {
    val mockList = List(3, 2, 5, 7, 1)
    val symmetricTreeList = List(5, 3, 18, 1, 4, 12, 21)
    Tree.fromList(mockList).toString should be("T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))")
    Tree.fromList(symmetricTreeList).isSymmetric should be(true)
  }

  "symmetricBalancedTress" should "construct all symmetric, completely balanced binary trees " +
    "with a given number of nodes" in {
    Tree.symmetricBalancedTrees(5, "x").mkString(", ") should be(
      "T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .)))"
    )
  }
}
