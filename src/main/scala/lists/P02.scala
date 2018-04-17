package lists

import java.util.NoSuchElementException

object P02 {

  def penultimate[A](a: List[A]): A = a match {
    case h :: _ :: Nil => h
    case _ :: t        => penultimate(t)
    case _             => throw new NoSuchElementException()
  }


  def penultimateN[A](a: List[A], n: Int): A = {
    val dropped = a.drop(n)
    lazy val zipped = a.zipAll(dropped, None, None)

    def penultimateHelper(zippedArray: List[(Any, Any)]): A = zippedArray match {
      case (last: A, None) :: _ => last
      case (_: A, _: A) :: t    => penultimateHelper(t)
      case _                    => throw new NoSuchElementException
    }

    dropped match {
      case Nil => throw new NoSuchElementException
      case _   => penultimateHelper(zipped)
    }
  }


  def penultimateNBuiltIn[A](a: List[A], n: Int): A = {
    if (n <= 0) throw new IllegalArgumentException
    else if (a.length < n) throw new NoSuchElementException
    a.takeRight(n).head
  }

  def penultimateNOptimized[A](a: List[A], n: Int): A = {
    def penultimateHelper(count: Int, resultList: List[A], curList: List[A]): A = {
      curList match {
        case _ :: tail        => penultimateHelper(count - 1,
          if (count > 0) resultList else resultList.tail,
          tail)
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil              => resultList.head
      }
    }

    if (n <= 0) throw new IllegalArgumentException
    else penultimateHelper(n, a, a)
  }
}
