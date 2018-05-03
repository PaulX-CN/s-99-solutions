package arithmetic


object P38 {

  import P31nP33toP37nP40._

  private val testNums = List(300, 3000, 30000)

  def testTotientImproved: List[Int] = testNums.map(_.totientImproved)

  def testTotient: List[Int] = testNums.map(_.totient)
}
