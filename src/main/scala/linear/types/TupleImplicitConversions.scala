package linear.types

import linear.{%:, Empty}

/**
 * Represents implicit conversions from homogenous tuples to [[linear.OrdSet]]s.
 */
trait TupleImplicitConversions {

  import scala.language.implicitConversions

  implicit def fromT1[T](t: Tuple1[T]): T %: Empty.type = t._1 %: Empty
  implicit def fromT2[T](t: (T, T)): T %: T %: Empty.type = t._1 %: t._2 %: Empty
  implicit def fromT3[T](t: (T, T, T)): T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: Empty
  implicit def fromT4[T](t: (T, T, T, T)): T %: T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: t._4 %: Empty
  implicit def fromT5[T](t: (T, T, T, T, T)): T %: T %: T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: t._4 %: t._5 %: Empty

}
