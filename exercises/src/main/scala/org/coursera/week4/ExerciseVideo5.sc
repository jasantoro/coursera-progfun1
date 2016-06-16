trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

object number {
  def apply(n: Int) = new Number(n)
}

object sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}

object prod {
  def apply(e1: Expr, e2: Expr) = new Prod(e1, e2)
}

object variable {
  def apply(name: String) = new Var(name)
}

def eval(e: Expr): Int = e match {
  case Number(x) => x
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Prod(e1: Sum, e2) => "(" + show(e1) + ") * " + show(e2)
  case Prod(e1, e2) => show(e1) + " * " + show(e2)
  case Var(x) => x
}

eval(Sum(Number(1), Number(2)))
show(Sum(Number(1), Number(2)))

eval(Prod(Number(2), Number(3)))
show(Prod(Number(2), Number(3)))

show(Prod(Sum(Number(2), Var("x")), Var("y")))
show(Sum(Prod(Number(2), Var("x")), Var("y")))