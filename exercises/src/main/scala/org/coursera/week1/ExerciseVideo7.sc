import scala.annotation.tailrec

object ExerciseVideo7 {

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if(b == 0) a else gcd(b, a % b)

  def noTailRecfactorial(n: Int) : Int =
    if(n == 0) 1 else n * noTailRecfactorial(n - 1)

  def factorial(n: Int) : Int = {
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  gcd(21, 14)
  noTailRecfactorial(4)
  factorial(4)
}
