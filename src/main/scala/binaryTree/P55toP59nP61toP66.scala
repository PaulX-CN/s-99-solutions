package binaryTree

import scala.util.matching.Regex

object P55toP59nP61toP66 {

  sealed trait Tree[+T]

  /** Defining all the solutions in a TreeExtension abstract class, unlike the solutions
    * provided by the author because for Problem 64 we want to define another case class
    * PositionedBranch but we don't need all the functions associated with it.
    *
    * @note Most importantly the original solution to Problem 64 is outdated since
    * case to case inheritance has been prohibited in Scala
    *
    **/
  sealed abstract class TreeExtension[+T] extends Tree[T] {

    def toString2: String

    def isSymmetric: Boolean

    /** define abstract method to construct new binary tree by adding new item
      *
      * @note using contravariant for type U because we want to allow to
      * construct a new tree of new type by adding a new value that
      * is super type to current T type
      * @return new tree of provided type
      * */
    def addValue[U >: T](x: U)(implicit oc: U => Ordered[U]): TreeExtension[U]

    def depth: Int

    def leftmostNodeDepth: Int

    def nodeCount: Int

    def leafCount: Int

    def leafList: List[T]

    def internalList: List[T]

    def atLevel(n: Int): List[T]

    def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1

    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)

    def layoutBinaryTree2: Tree[T] = {

      // Tree depth
      val d = depth
      // Starting from left most tree, add the width all the way back up to the root
      val x0 = (2 to leftmostNodeDepth).map(n => Math.pow(2, d - n).toInt).sum + 1

      layoutBinaryTreeInternal2(x0, 1, d - 2)
    }

    def layoutBinaryTreeInternal2(x: Int, depth: Int, exp: Int): Tree[T]

  }

  case class Branch[+T](value: T, left: TreeExtension[T], right: TreeExtension[T]) extends TreeExtension[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def toString2: String = (left, right) match {
      case (Leaf, Leaf) => s"$value"
      case (_, _) => s"$value(${left.toString2},${right.toString2})"
    }

    def isSymmetric: Boolean = isMutalMirror(left, right)

    def addValue[U >: T](x: U)(implicit oc: U => Ordered[U]): TreeExtension[U] = {
      if (x >= value) Branch(value, left, right.addValue(x))
      else Branch(value, left.addValue(x), right)
    }

    def nodeCount: Int = left.nodeCount + right.nodeCount + 1

    def depth: Int = (left.depth max right.depth) + 1

    def leftmostNodeDepth: Int = left.leftmostNodeDepth + 1

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

    /** Return the layout of current node and X for parent tree to use
      * returned X becomes the x value of parent node.
      * This is a recursive solution and apparently it will stack over flow.
      *
      * @param x : starting x for left tree
      * @param depth : current depth. Subtree will have depth + 1
      * @return (Tree[T], Int): layout of current node and next X
      *
      **/
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
      // get left tree's layout and let left tree determine what the current X is
      val (leftTree, currentX) = left.layoutBinaryTreeInternal(x, depth + 1)
      // get right tree's layout and let right tree determine what the next X should be
      val (rightTree, nextX) = right.layoutBinaryTreeInternal(currentX + 1, depth + 1)

      (PositionedBranch(value, leftTree, rightTree, currentX, depth), nextX)
    }


    /** Given the starting depth and starting exponential (which is the total depth-2)
      * to construct the tree
      *
      * @param x : current X for the node
      * @param depth : starting depth which is the y axis
      * @param exp : exponential number calculated from the total depth
      *
      **/
    def layoutBinaryTreeInternal2(x: Int, depth: Int, exp: Int): Tree[T] = {
      PositionedBranch(
        value,
        left.layoutBinaryTreeInternal2(x - Math.pow(2, exp).toInt, depth + 1, exp - 1),
        right.layoutBinaryTreeInternal2(x + Math.pow(2, exp).toInt, depth + 1, exp - 1),
        x, depth)
    }

  }

  case class PositionedBranch[+T](value: T, left: Tree[T], right: Tree[T], x: Int, y: Int) extends Tree[T] {
    override def toString: String = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object Leaf extends TreeExtension[Nothing] {
    override def toString = "."

    def toString2: String = ""

    def isSymmetric = true

    def addValue[U >: Nothing](x: U)(implicit oc: U => Ordered[U]): TreeExtension[U] = Branch(x)

    def nodeCount = 0

    def depth = 0

    def leftmostNodeDepth: Int = 0

    def leafCount = 0

    def leafList = Nil

    def internalList = Nil

    def atLevel(n: Int) = Nil

    // Leaf does not use X so whatever X passed to Leaf just pass it back
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[Nothing], Int) = (Leaf, x)

    def layoutBinaryTreeInternal2(x: Int, depth: Int, exp: Int): Tree[Nothing] = Leaf

  }

  object Branch {
    def apply[T](value: T): Branch[T] = new Branch(value, Leaf, Leaf)
  }


  /** check if two trees are mirror of each other
    * */
  def isMutalMirror[A](a: TreeExtension[A], b: TreeExtension[A]): Boolean = (a, b) match {
    case (Leaf, Leaf) => true
    case (Branch(_, aLeft, aRight), Branch(_, bLeft, bRight)) =>
      isMutalMirror(aLeft, bRight) && isMutalMirror(aRight, bLeft)
    case _ => false
  }

  object TreeExtension {

    def fromString(st: String): TreeExtension[Char] = {

      /** Return three tree parts given a tree starting number in format of
        * (leftTreeString, index of comma, rightTreeString)
        *
        * @param start : starting index of the 1st left bracket */
      def findTreeParts(start: Int): (String, Int, String) = {

        def findTreeEnds(pos: Int, nesting: Int, end: Char): Int =
          if (st(pos) == end && nesting == 0) pos
          else if (st(pos) == '(') findTreeEnds(pos + 1, nesting + 1, end)
          else if (st(pos) == ')') findTreeEnds(pos + 1, nesting - 1, end)
          else findTreeEnds(pos + 1, nesting, end)

        val leftTreeEnd = findTreeEnds(start, 0, ',')
        val rightTreeEnd = findTreeEnds(leftTreeEnd + 1, 0, ')')

        (st.substring(start, leftTreeEnd), leftTreeEnd, st.substring(leftTreeEnd + 1, rightTreeEnd))
      }

      if (st.length == 0) Leaf
      else if (st.length == 1) Branch(st(0))
      else {
        val (leftString, comma, rightString) = findTreeParts(2)
        // Basic recursive
        Branch(st(0), fromString(leftString), fromString(rightString))
      }
    }

    def cBalance[T](n: Int, v: T): List[TreeExtension[T]] = n match {
      case n if n == 0 => List(Leaf)
      case n if n % 2 == 0 =>
        val lightTree = cBalance((n - 1) / 2, v)
        val heavyTree = cBalance((n - 1) / 2 + 1, v)
        lightTree.flatMap { lt => heavyTree.flatMap { ht => List(Branch(v, lt, ht), Branch(v, ht, lt)) } }
      case n if n % 2 == 1 =>
        val subtree = cBalance(n / 2, v)
        subtree.flatMap { l => subtree.map { r => Branch(v, l, r) } }
    }

    def fromList[T](ls: List[T])(implicit oc: T => Ordered[T]): TreeExtension[T] =
      ls.foldLeft(Leaf: TreeExtension[T])((oldTree, x) => oldTree.addValue(x))

    def symmetricBalancedTrees[T](n: Int, c: T): List[TreeExtension[T]] = {
      TreeExtension.cBalance(n, c).filter(t => t.isSymmetric)
    }

    def hbalTress[T](n: Int, v: T): List[TreeExtension[T]] = {
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


    /** Generate a complete binary tree by counting the address of each node.
      * If the address is larger than the number of nodes given, then stop.
      *
      * @param nodes : number of nodes in the tree
      * @param value : value of each node
      **/
    def completeBinaryTree[T](nodes: Int, value: T): TreeExtension[T] = {
      def generateTree(address: Int): TreeExtension[T] = {
        if (address > nodes) Leaf
        else Branch(value, generateTree(2 * address), generateTree(2 * address + 1))
      }

      // Starting address is 1 for the root node
      generateTree(1)
    }
  }

}
