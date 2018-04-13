import main.scala.P23NP24NP25P26._
import org.scalatest._
import org.scalatest.tagobjects.Retryable

class testP23NP24NP25P26 extends FlatSpec with Matchers with Retries {

  val retries = 50

  override def withFixture(test: NoArgTest) = {
    if (isRetryable(test)) withFixture(test, retries) else super.withFixture(test)
  }

  def withFixture(test: NoArgTest, count: Int): Outcome = {
    val outcome = super.withFixture(test)
    outcome match {
      case Failed(_) | Canceled(_) => if (count == 1) super.withFixture(test) else withFixture(test, count - 1)
      case other                   => other
    }
  }

  "randomSelect" should "random select n elements from list without duplicates" in {
    val mockList = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    randomSelect(3, mockList).length should be(3)
    randomSelect(3, mockList).distinct.size should be(3)
  }

  "lotto" should "random pick n elements from range of 1 to end" taggedAs Retryable in {
    lotto(3, 40).length should be(3)
    lotto(3, 40).distinct.size should be(3)
    lotto(3, 40).max should be <= 40
    lotto(3, 40).min should be >= 1
  }

  "randomPermute" should "generate a random permutation of given list" in {
    val mockList = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    randomPermute(mockList).toSet should be(mockList.toSet)
  }

  "combination" should "generate all combinations of n elements from a given list" in {
    val mockList = List('a, 'b, 'c, 'd)
    combination(4, mockList).toSet should be(Set(List('a, 'b), List('a, 'c), List('b, 'c),
                                                  List('a, 'd), List('b, 'd), List('c, 'd)))
  }

}
