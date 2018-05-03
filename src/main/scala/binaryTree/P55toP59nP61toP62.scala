package binaryTree

object P55toP59nP61toP62 {

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

    def nodeCount: Int

    def leafCount: Int

    def leafList: List[T]

    def internalList: List[T]

    def atLevel(n: Int): List[T]
  }

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def isSymmetric: Boolean = isMutalMirror(left, right)


    def addValue[U >: T](x: U)(implicit oc: U => Ordered[U]): Tree[U] = {
      if (x >= value) Branch(value, left, right.addValue(x))
      else Branch(value, left.addValue(x), right)
    }

    def nodeCount: Int = left.nodeCount + right.nodeCount + 1

    def leafCount: Int = (left, right) match {
      case (Leaf, Leaf) => 1
      case _ => left.leafCount + right.leafCount
    }

    def leafList: List[T] = (left, right) match {
      case (Leaf, Leaf) => List(value)
      case _ => left.leafList ::: right.leafList
    }

    def internalList: List[T] = (left, right) match {
      case (Leaf, Leaf) => Nil
      case _ => value :: left.internalList ::: right.internalList
    }

    def atLevel(n: Int): List[T] = (left, right, n) match {
      case (_, _, x) if x == 1 => List(value)
      case _ => left.atLevel(n - 1) ::: right.atLevel(n - 1)
    }
  }

  case object Leaf extends Tree[Nothing] {
    override def toString = "."

    def isSymmetric = true

    def addValue[U >: Nothing](x: U)(implicit oc: U => Ordered[U]): Tree[U] = Branch(x)

    def nodeCount = 0

    def leafCount = 0

    def leafList = Nil

    def internalList = Nil

    def atLevel(n: Int) = Nil
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

    def hbalTress[T](n: Int, v: T): List[Tree[T]] = {
      if (n == 0) List(Leaf)
      else if (n == 1) List(Branch(v))
      else {
        val fullHeightTreeOneLevelLess = hbalTress(n - 1, v)
        val shorterTreeTwoLevelLess = hbalTress(n - 2, v)
        val completeBalancedTrees = for {
          left <- fullHeightTreeOneLevelLess
          right <- fullHeightTreeOneLevelLess
        } yield Branch(v, left, right)
        val almostBalancedTrees =
          shorterTreeTwoLevelLess.flatMap { l =>
            fullHeightTreeOneLevelLess.flatMap {
              r => List(Branch(v, l, r), Branch(v, r, l))
            }
          }
        completeBalancedTrees ::: almostBalancedTrees
      }
    }
  }

}
