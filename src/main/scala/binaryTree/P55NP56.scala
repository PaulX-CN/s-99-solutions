package binaryTree

object P55NP56 {

  sealed abstract class Tree[+T] {
    def isSymmetric: Boolean
  }

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isSymmetric: Boolean = isMutalMirror(left, right)
  }

  case object Leaf extends Tree[Nothing] {
    override def toString = "."

    override def isSymmetric = true
  }

  object Branch {
    def apply[T](value: T): Branch[T] = new Branch(value, Leaf, Leaf)
  }

  def isMutalMirror[A](a: Tree[A], b: Tree[A]): Boolean = (a, b) match {
    case (Leaf, Leaf) => true
    case (Branch(_, aLeft, aRight), Branch(_, bLeft, bRight)) =>
      isMutalMirror(aLeft, bRight) && isMutalMirror(aRight, bLeft)
    case _ => false
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
