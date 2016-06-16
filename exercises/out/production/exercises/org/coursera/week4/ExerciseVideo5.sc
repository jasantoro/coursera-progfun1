trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object number {
  def apply(n: Int) = new Number(n)
}

object sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}

object prod {
  def apply(e1: Expr, e2: Expr) = new Prod(e1, e2)
}

def eval(e: Expr): Int = e match {
  case Number(x) => x
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

eval(Sum(Number(1), Number(2)))

eval(Prod(Number(2), Number(3)))