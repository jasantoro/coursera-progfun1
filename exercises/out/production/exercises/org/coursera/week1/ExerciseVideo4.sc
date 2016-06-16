object ExerciseVideo4 {

  def loop: Boolean = loop

  def and(x: Boolean, y: => Boolean): Boolean =
    if (x) y else false

  and(true, false)
  and(true, true)

  and(false, loop)
}
