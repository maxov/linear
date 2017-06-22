package linear

import linear.types.Dim

trait Vector {
  type D <: Dim
}

trait Space { space =>

  type D <: Dim

  type V <: Vector

  def zero: V

  def plus(v1: V, v2: V): V

  protected implicit def mkOps(v: V): Ops

  trait Ops {
    def v: V
    def unary_-(): V
    def +(that: V): V
    def -(that: V): V = v + -that
  }

}

object Space {

  type ofDim[Di <: Dim] = Space { type Di = Dim }

}