package main.scala

object P28NP29 {

  def lsort[A](ls: List[List[A]]): List[List[A]] = {
    ls.sortBy(_.length)
  }

  def quickSort[A, B](ls: List[A])(f: A => B)(implicit ev: B => Ordered[B]): List[A] = ls match {
    case Nil          => Nil
    case a :: Nil     => List(a)
    case head :: tail => {
      quickSort(ls.filter(f(_) < f(head)))(f) :::
        ls.filter(f(_) == f(head)) :::
        quickSort(ls.filter(f(_) > f(head)))(f)
    }
  }

  def lsortByQuickSort[A](ls: List[List[A]]): List[List[A]] = {
    quickSort(ls)(ls => ls.length)
  }

  def sortByFreq[A](ls: List[List[A]]): List[List[A]] = {
    val freqMap = ls.groupBy(_.length).mapValues(_.size)
    quickSort(ls) { sublist =>
      freqMap(sublist.length)
    }
  }
}
