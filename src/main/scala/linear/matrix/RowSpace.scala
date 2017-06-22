package linear.matrix

import linear.OrdSet.ofDim
import linear._

trait RowSpace extends Space {

  implicit def dimV: DimVal[D]

  type D <: Dim
  type V = OrdSet.ofDim[Double, D]

  def zero: V = OrdSet.fill[Double, D](0)

  private[RowSpace] def unsafePlus(v1: OrdSet[Double], v2: OrdSet[Double]): OrdSet[Double] = (v1, v2) match {
    case (Empty, Empty) => Empty
    case (a1 %: r1, a2 %: r2) => (a1 + a2) %: unsafePlus(r1, r2)
    case _ => throw new IllegalArgumentException
  }

  private[RowSpace] def unsafeScal(v: OrdSet[Double], k: Double): OrdSet[Double] = v match {
    case Empty => Empty
    case (a1 %: r1) => (a1 * k) %: unsafeScal(r1, k)
  }

  def plus(v1: V, v2: V): V = unsafePlus(v1, v2).asInstanceOf[V]
  def scale(v: V, k: Double): V = unsafeScal(v, k).asInstanceOf[V]

  trait Ops extends super.Ops {
    def *(that: Double): V
  }

  override implicit def mkOps(ve: V): Ops = new Ops {
    def v: V = ve
    def *(that: Double): V = scale(v, that)
    def unary_-(): V = scale(v, -1)
    def +(that: V): V = plus(v, that)
  }

}

object RowSpace {
  type ofDim[Di <: Dim] = RowSpace { type D = Di }
  def apply[Di <: Dim](implicit dv: DimVal[Di]): ofDim[Di] = new RowSpace {
    type D = Di
    def dimV: DimVal[Di] = dv
  }
}