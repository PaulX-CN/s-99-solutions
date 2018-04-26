package logic

import org.scalatest.{FlatSpec, Matchers}
import logic.P50._

class testP50 extends FlatSpec with Matchers {

  "constructTree" should "return a tree based on given leaves" in {
    val mockLeaf = List(Leaf(None, 2))
    constructTree(Leaf(None, 0), mockLeaf) should be(Leaf(None, 2))

    val mockLeaves = List(Leaf(None, 3), Leaf(None, 1), Leaf(None, 4))
    constructTree(Leaf(None, 0), mockLeaves) should be(
      Branch(
        Branch(Leaf(None, 1),
          Leaf(None, 3), 4)
        , Leaf(None, 4), 8)
    )
  }

  "decodeTree" should "return bala" in {
    val mockTree = Branch(
      Branch(Leaf(None, 1),
        Leaf(None, 3), 4)
      , Leaf(None, 4), 8)
    decodeTree(mockTree, 0) should be(List("a", 1))
  }
}
