package linear.matrix

import linear.{Dim, DimVal, OrdSet, Space}

trait RowSpace extends Space {

  type D <: Dim
  type V = OrdSet.ofDim[Double, D]

  def zero: V = OrdSet.fill[Double, D](0)
}

object RowSpace {
  type ofDim[Di <: Dim] = RowSpace { type D = Di }
  def apply[Di <: Dim](implicit dv: DimVal[Di]): ofDim[Di] = new RowSpace {
    type D = Di
    def dimV: DimVal[Di] = dv
  }
}