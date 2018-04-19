package arithmetic

object P32 {

  def gcd(a: Int, b: Int):Int = {
    if (a % b == 0) b
    else gcd(b, a % b)
  }
}
