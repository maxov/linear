package linear.types

import linear.{%:, Empty, OrdSet}

trait Rec[A] {
  type Base <: A
  type Succ[X <: A] <: A
}

object Rec {
  def ordSet[T] = new Rec[OrdSet[T]] {
    type Base = Empty.type
    type Succ[X <: OrdSet[T]] = T %: X
  }
}