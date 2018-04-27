package binaryTree

object P55 {

  sealed abstract class Tree[+T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object Leaf extends Tree[Nothing] {
    override def toString = "."
  }

  object Branch {
    def apply[T](value: T): Branch[T] = new Branch(value, Leaf, Leaf)
  }

  object Tree {
    def cBalance[T](n: Int, v: T): List[Tree[T]] = n match {
      case n if n == 0 => List(Leaf)
      case n if n % 2 == 0 =>
        val lightTree = cBalance((n - 1) / 2, v)
        val heavyTree = cBalance((n - 1) / 2 + 1, v)
        lightTree.flatMap { lt => heavyTree.flatMap { ht => List(Branch(v, lt, ht), Branch(v, ht, lt)) } }
      case n if n % 2 == 1 =>
        val subtree = cBalance(n / 2, v)
        subtree.flatMap { l => subtree.map { r => Branch(v, l, r) } }
    }
  }

}
