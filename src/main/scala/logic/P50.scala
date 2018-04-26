package logic

import scala.annotation.tailrec

object P50 {

  sealed trait Tree {
    def getValue: Int
  }

  case class Leaf(name: Option[String], xvalue: Int) extends Tree {
    def getValue: Int = xvalue
  }

  case class Branch(left: Tree, right: Tree, xvalue: Int) extends Tree {
    def getValue: Int = xvalue
  }


  @tailrec
  def constructTree(currentTree: Tree, remainLeaves: List[Tree]): Tree = remainLeaves match {
    case h :: Nil => h
    case _ =>
      val sortedLeaves = remainLeaves.sortBy { x => x.getValue }
      val leftLeaf = sortedLeaves.head
      val rightLeaf = sortedLeaves(1)
      val newTree = Branch(leftLeaf, rightLeaf, leftLeaf.getValue + rightLeaf.getValue)
      constructTree(newTree,
        newTree :: sortedLeaves.slice(2, sortedLeaves.size))
  }

  def decodeTree(tree: Tree, depth: Int): List[(String, Int)] = tree match {
    case Leaf(n, v) => List((n.getOrElse(""), depth))
    case Branch(l, r, v) => decodeTree(l, depth + 1) ::: decodeTree(r, depth + 1)
  }

  def huffman(ls: List[(String, Int)]): List[(String, String)] = {

    List(("a", "a"))
  }

}
