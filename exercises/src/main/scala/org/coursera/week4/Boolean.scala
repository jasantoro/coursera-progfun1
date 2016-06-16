package org.coursera.week4

/**
  * Created by jorge on 15/06/16.
  */
abstract class IdealizedBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => IdealizedBoolean): IdealizedBoolean = ifThenElse(x, False)
  def || (x: => IdealizedBoolean): IdealizedBoolean = ifThenElse(True, x)
  def unary_! : IdealizedBoolean = ifThenElse(False, True)

  def == (x: => IdealizedBoolean): IdealizedBoolean = ifThenElse(x, x.unary_!)
  def != (x: => IdealizedBoolean): IdealizedBoolean = ifThenElse(x.unary_!, x)

  def < (x: => IdealizedBoolean): IdealizedBoolean = ifThenElse(False, x)

  object True extends IdealizedBoolean {
    override def ifThenElse[T](t: => T, e: => T): T = t
  }
  object False extends IdealizedBoolean {
    override def ifThenElse[T](t: => T, e: => T): T = e
  }
}
