package linear

import linear.types.Rec

sealed trait Dim {
  type Build[A, R <: Rec[A]] <: A
}

trait DimVal[D <: Dim] {
  def value: Int
}

object Dim {

  private[Dim] final case object _0 extends Dim {
    type Build[A, R <: Rec[A]] = R#Base
  }
  type _0 = _0.type
  final class Succ[T <: Dim] extends Dim {
    type Build[A, R <: Rec[A]] = R#Succ[T#Build[A,R]]
  }

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]

  implicit object Zero extends DimVal[_0] {
    def value = 0
  }

  implicit def Succ[D <: Dim](implicit prev: DimVal[D]): DimVal[Succ[D]] = new DimVal[Succ[D]] {
    override def value: Int = prev.value + 1
  }

  def v[D <: Dim](implicit v: DimVal[D]): Int = v.value

}