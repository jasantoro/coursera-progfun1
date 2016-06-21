def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

def scaleList2(xs: List[Double], factor: Double): List[Double] = xs map (x => x * factor)

scaleList(List(2, 4), 4)
scaleList2(List(2, 4), 4)

def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y * y :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs map (x => x * x)

squareList(List(2, 4))
squareList2(List(2, 4))

def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if(y > 0) y :: posElems(ys) else posElems(ys)
}

def posElems2(xs: List[Int]): List[Int] = xs filter (x => x > 0)

posElems(List(2, 3, -1, 0))
posElems2(List(2, 3, -1, 0))

val nums = List(2, -4 ,5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

pack(List("a","a","a","b","c","c","a"))

def encode[T](xs: List[T]): List[(T,Int)] =
  pack(xs) map (ys => (ys.head, ys.size))

encode(List("a","a","a","b","c","c","a"))
