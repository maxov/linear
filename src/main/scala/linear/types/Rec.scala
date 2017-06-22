package linear.types

trait Rec[A] {
  type Base <: A
  type Succ[X <: A] <: A
}