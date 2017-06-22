package linear.matrix

import linear.{Dim, OrdSet, Space}

class RowSpace extends Space {

  type D <: Dim
  type V = OrdSet.ofDim[Double, D]

  def zero: V = null
}

object RowSpace {
  type ofDim[Di <: Dim] = RowSpace { type D = Di }
}