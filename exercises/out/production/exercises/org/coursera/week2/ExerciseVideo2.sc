object ExerciseVideo2 {

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if(a > b) 0
    else f(a) + sum(f)(a+ 1, b)

  sum(x => x)(1, 4)

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * product(f)(a+ 1, b)

  product(x => x)(1, 4)

  def fact(n: Int): Int = product(x => x)(1, n)

  fact(3)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if(a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+ 1, b))

  mapReduce(x=>x, (a,b)=> a+b, 0)(x=>x)(1,4)
  mapReduce(x=>x, (a,b)=> a*b, 1)(x=>x)(1,4)

}
