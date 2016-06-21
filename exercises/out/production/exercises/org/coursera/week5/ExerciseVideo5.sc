def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft (_ + _)
def prod(xs: List[Int]): Int = (1 :: xs) reduceLeft (_ * _)

sum(List(1,2,3,4))
prod(List(1,2,3,4))

def sum2(xs: List[Int]): Int = (xs foldLeft 0) (_ + _)
def prod2(xs: List[Int]): Int = (xs foldLeft 1) (_ * _)

sum2(List(1,2,3,4))
prod2(List(1,2,3,4))

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)

concat(List(1,2,3), List(4,5,6))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x, y) => y + 1)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) ((x, y) => f(x) :: )

lengthFun(List[Int](1,2,3))
mapFun[Int,Int](List[Int](1,2,3), (_ + 1))