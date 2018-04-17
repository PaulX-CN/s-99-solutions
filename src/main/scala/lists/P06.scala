package lists

object P06 {

  def isPalindrome[A](a: List[A]): Boolean = {
    a == a.reverse
  }

  def isPalindromeRecursive[A](a: List[A]): Boolean = {
    def _palindrome(res: Boolean, l: List[A]): Boolean = l match {
      case Nil                     => res
      case frl(head, middle, last) => _palindrome(res && head == last, middle)
    }

    _palindrome(true, a)
  }


  object frl {
    def unapply[A](l: List[A]) = l match {
      case Nil                => None
      case l if l.length == 1 => Some(l.head, Nil, l.last)
      case l                  => Some(l.head, l.init.tail, l.last)
    }
  }

}
