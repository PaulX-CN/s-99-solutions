package logic

import org.scalatest.{FlatSpec, Matchers}
import logic.P50._

class testP50 extends FlatSpec with Matchers {

  "constructTree" should "return a tree based on given leaves" in {
    val mockLeaf = List(Leaf(None, 2))
    constructTree(mockLeaf) should be(Leaf(None, 2))

    val mockLeaves = List(Leaf(None, 3), Leaf(None, 1), Leaf(None, 4))
    constructTree(mockLeaves) should be(
      Branch(
        Branch(Leaf(None, 1),
          Leaf(None, 3), 4)
        , Leaf(None, 4), 8)
    )
  }

  "decodeTree" should "return decoded sequence of a tree" in {
    val mockTree = Branch(
      Branch(Leaf(None, 1),
        Leaf(None, 3), 4)
      , Leaf(None, 4), 8)
    decodeTree(mockTree, "") should be(List(("a", "1")))
  }

  "huffman" should "return huffman code of given name->frequence pair list" in {
    val mockList = List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
    huffman(mockList).toSet should be(Set(("a","0"), ("b","101"), ("c","100"), ("d","111"), ("e","1101"), ("f","1100")))
  }
}
