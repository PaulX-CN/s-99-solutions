package logic

object P46nP47 {


  // a native approach. Removed implicit marker to avoid conflicts
   class BooleanExtensionNative(a: Boolean) {

    def and(b: Boolean):Boolean = a && b

    def or(b: Boolean):Boolean = a || b

    def xor(b: Boolean):Boolean = a != b

    def nand(b: Boolean):Boolean = ! (a&&b)

    def nor(b: Boolean):Boolean = !a && !b

    def impl(b: Boolean):Boolean = (a && b) || (!a) // a and b are both true or a is false

    def equ(b: Boolean): Boolean = a == b
  }

  implicit class BooleanExtension(a: Boolean) {

    def not: Boolean = a match {
      case true => false
      case false => true
    }

    def and(b: Boolean):Boolean = (a,b) match {
      case (true, true) => true
      case _ => false
    }

    def or(b: Boolean):Boolean = (a,b) match {
      case (true, _) => true
      case (_, true) => true
      case _ => false
    }

    def equ(b: Boolean): Boolean = (a and b) or (a.not and b.not)

    def xor(b: Boolean):Boolean = (a equ b).not

    def nand(b: Boolean):Boolean = (a and b).not

    def nor(b: Boolean):Boolean = a.not and b.not

    def impl(b: Boolean):Boolean = (a and b) or a.not // a and b are both true or a is false

  }

  def table2(f: (Boolean, Boolean) => Boolean): List[(Boolean, Boolean, Boolean)] = {
    for {
      a <- List(true, false)
      b <- List(true, false)
    } yield (a, b, f(a, b))
  }
}
