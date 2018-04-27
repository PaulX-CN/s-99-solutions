package logic

import scala.annotation.tailrec

object P50 {

  sealed trait Tree {
    def getValue: Int
  }

  case class Leaf(name: Option[String], frequency: Int) extends Tree {
    def getValue: Int = frequency
  }

  case class Branch(left: Tree, right: Tree, frequency: Int) extends Tree {
    def getValue: Int = frequency
  }


  /** Function to construct the tree for decoding to Huffman Code.
    *
    * @param remainLeaves: all leaves (could also be a branch) left to construct the tree.
    *
    * */
  @tailrec
  def constructTree(remainLeaves: List[Tree]): Tree = remainLeaves match {
    case h :: Nil => h
    case _ =>
      // TODO: could optimize to insert new Tree at correct place since the array is always sorted
      val sortedLeaves = remainLeaves.sortBy { x => x.getValue }
      val leftLeaf = sortedLeaves.head
      val rightLeaf = sortedLeaves(1)
      val newTree = Branch(leftLeaf, rightLeaf, leftLeaf.getValue + rightLeaf.getValue)
      constructTree(newTree :: sortedLeaves.slice(2, sortedLeaves.size))
  }


  def decodeTree(tree: Tree, base: String): List[(String, String)] = tree match {
    case Leaf(n, v) => List((n.getOrElse(""), base))
    case Branch(l, r, v) => decodeTree(l, base + "0") ::: decodeTree(r, base + "1")
  }


  def huffman(ls: List[(String, Int)]): List[(String, String)] = {
    val leaves = ls.map(pair => Leaf(Some(pair._1), pair._2))
    val tree = constructTree(leaves)
    decodeTree(tree, "")
  }

}
