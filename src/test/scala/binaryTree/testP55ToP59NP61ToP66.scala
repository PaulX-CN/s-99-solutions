package binaryTree

import binaryTree.P55toP59nP61toP66._
import org.scalatest.{FlatSpec, Matchers}

class testP55ToP59NP61ToP66 extends FlatSpec with Matchers {

  "cBalance" should "return list of all possible complete balanced tree for a given number of nodes" in {
    TreeExtension.cBalance(4, "x") should contain(
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
    TreeExtension.fromList(mockList).toString should be("T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))")
    TreeExtension.fromList(symmetricTreeList).isSymmetric should be(true)
  }

  "symmetricBalancedTress" should "construct all symmetric, completely balanced binary trees " +
    "with a given number of nodes" in {
    TreeExtension.symmetricBalancedTrees(5, "x").mkString(", ") should be(
      "T(x T(x . T(x . .)) T(x T(x . .) .)), T(x T(x T(x . .) .) T(x . T(x . .)))"
    )
  }

  "hbalTrees" should "construct height-balanced binary trees for a given height " +
    "with a supplied value for the nodes" in {
    TreeExtension.hbalTress(3, "x").map(_.toString) should contain(
      "T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .)))"
    )
  }

  "leafCount" should "return number of leaves in a tree" in {
    val mockTree = Branch('a', Branch('b'), Leaf)
    mockTree.leafCount should be(1)
  }

  "leafList" should "return an array of all nodes without successors" in {
    val mockTree = Branch('a', Branch('b'), Branch('c', Branch('d'), Branch('e')))
    mockTree.leafList should be(List('b', 'd', 'e'))
  }

  "internalList" should "return an array of all nodes has either one or two non-empty successors" in {
    val mockTree = Branch('a', Branch('b'), Branch('c', Branch('d'), Branch('e')))
    mockTree.internalList should be(List('a', 'c'))
  }

  "atLevel" should "collect all nodes at a given level in a list." in {
    val mockTree = Branch('a', Branch('b'), Branch('c', Branch('d'), Branch('e')))
    mockTree.atLevel(2) should be(List('b', 'c'))
  }

  "completeBinaryTree" should "generate a complete binary tree by giving " +
    "the number of nodes and the value to put in each node." in {
    TreeExtension.completeBinaryTree(6, "x").toString should be("T(x T(x T(x . .) T(x . .)) T(x T(x . .) .))")
  }

  "layoutBinaryTree" should "turns a tree of normal Nodes into a tree of PositionedNodes" in {
    val mockTree = Branch('a', Branch('b', Leaf, Branch('c')), Branch('d'))
    mockTree.layoutBinaryTree.toString should be("T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))")
  }

  "layoutBinaryTree2" should "turns a tree of normal Nodes into a tree of PositionedNodes" in {
    val mockTree = Branch('a', Branch('b', Leaf, Branch('c')), Branch('d'))
    mockTree.layoutBinaryTree2.toString should be("T[3,1](a T[1,2](b . T[2,3](c . .)) T[5,2](d . .))")
  }

  "layoutBinaryTree3" should "turns a tree of normal Nodes into a tree of PositionedNodes" in {
    val mockTree = Branch('a', Branch('b', Leaf, Branch('c')), Branch('d'))
    mockTree.layoutBinaryTree3.toString should be("T[2,1](a T[1,2](b . T[2,3](c . .)) T[3,2](d . .))")
  }
}
