def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init.emptyList")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last.emptyList")
  case List(x) => x
  case y :: ys => last(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs  drop n +1)

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = xs flatMap {
  case ms:List[_] => flatten(ms)
  case l => List(l)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))