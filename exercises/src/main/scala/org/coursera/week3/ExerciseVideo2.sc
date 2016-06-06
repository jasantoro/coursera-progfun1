import org.coursera.week3.Rational

object ExerciseVideo2 {
  new Rational(1, 2)

  trait Planar {
    def height: Int

    def width: Int

    def surface = height * width
  }

  class Square(x: Int, y: Int) extends Planar {
    override def height = x

    override def width = y
  }

  new Square(2, 3).surface
  
  val x = null
  val y: String = x

  if (true) 1 else false

  def error(msg: String) = throw new Error(msg)

  error("Error")
}

