package logic

import scala.annotation.tailrec

object P49 {

  /** A naive approach to generate next gray code.
    *
    * @param current : current code
    * @param changeType : type of changes need to happen
    * @return next gray code
    *
    **/
  def grayCodeGeneratorNaive(current: String, changeType: Int): Option[String] = {
    if (current.startsWith("1") && !current.substring(1).contains("1")) None
    else if (changeType == 1) {
      val head = current.dropRight(1)
      Some(head + (1 - current.takeRight(1).toInt))
    }
    else {
      val currentReverse = current.reverse
      val toReplaceIndex = currentReverse.indexOf("1") + 1
      val replaced = (currentReverse.substring(0, toReplaceIndex)
        + (1 - currentReverse.charAt(toReplaceIndex).asDigit)
        + currentReverse.substring(toReplaceIndex + 1)).reverse
      Some(replaced)
    }
  }

  def grayNaive(n: Int): List[String] = {

    @tailrec
    def _grayGenerator(currentList: List[String], currentType: Int): List[String] = {
      val next = grayCodeGeneratorNaive(currentList.head, 1 - currentType)
      next match {
        case None    => currentList.reverse
        case Some(x) => _grayGenerator(x :: currentList, 1 - currentType)
      }
    }

    val start = List.fill(n)("0").mkString
    _grayGenerator(List(start), 0)
  }

  def gray(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val previous = gray(n - 1)
      val reverse = previous.reverse
      previous.map(s => "0" + s) ::: reverse.map(s => "1" + s)
    }
  }

  import scala.collection.mutable

  private val grayList = mutable.Map(0 -> List(""))

  def grayMemorization(n: Int): List[String] = {
    if (!grayList.contains(n)) {
      grayList += (n -> ((grayMemorization(n - 1) map { "0" + _ }) :::
        (grayMemorization(n - 1).reverse map { "1" + _ })))
    }
    grayList(n)
  }

}
