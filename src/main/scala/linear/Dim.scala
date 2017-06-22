package linear

import linear.types.Rec

sealed trait Dim {
  type Build[A, R <: Rec[A]] <: A
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

}