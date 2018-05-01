package binaryTree

object P55TOP58 {

  sealed abstract class Tree[+T] {
    def isSymmetric: Boolean

    /** define abstract method to construct new binary tree by adding new item
      *
      * @note using contravariant for type U because we want to allow to
      *       construct a new tree of new type by adding a new value that
      *       is super type to current T type
      * @return new tree of provided type
      **/
    def addValue[U >: T](x: U)(implicit oc: U => Ordered[U]): Tree[U]
  }

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def isSymmetric: Boolean = isMutalMirror(left, right)


    def addValue[U >: T](x: U)(implicit oc: U => Ordered[U]): Tree[U] = {
      if (x >= value) Branch(value, left, right.addValue(x))
      else Branch(value, left.addValue(x), right)
    }
  }

  case object Leaf extends Tree[Nothing] {
    override def toString = "."

    def isSymmetric = true

    def addValue[U >: Nothing](x: U)(implicit oc: U => Ordered[U]): Tree[U] = Branch(x)
  }

  object Branch {
    def apply[T](value: T): Branch[T] = new Branch(value, Leaf, Leaf)
  }


  /** check if two trees are mirror of each other
    * */
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

    def fromList[T](ls: List[T])(implicit oc: T => Ordered[T]): Tree[T] =
      ls.foldLeft(Leaf: Tree[T])((oldTree, x) => oldTree.addValue(x))

    def symmetricBalancedTrees[T](n: Int, c: T): List[Tree[T]] = {
      Tree.cBalance(n, c).filter(t => t.isSymmetric)
    }
  }

}
