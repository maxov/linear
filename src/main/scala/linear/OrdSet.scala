package linear

import linear.types.Rec

import language.higherKinds
import language.implicitConversions
import scala.annotation.unchecked.uncheckedVariance

sealed trait OrdSet[+T] extends Vector {
  type Length <: Dim
  type D = Length
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]]
  def values: List[T]
  def mat[R, D <: Dim](implicit ev: T <:< OrdSet.ofDim[R, D]): Mat[R, Length, D] = null
  override def toString = values.mkString("(", ", ", ")")
}

case object Empty extends OrdSet[Nothing] {
  type Length = Dim._0
  def values = List()
  def map[B](f: Nothing => B): Empty.type = this
}

case class %:[+T, +R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type Length = Dim.Succ[tail.Length]
  def values: List[T] = head :: tail.values
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]] = (f(head) %: tail.map[B](f)).asInstanceOf[Length#Build[OrdSet[B], OrdSet.R[B]]]
}

trait ImplicitConversions {

  implicit def fromT1[T](t: Tuple1[T]): T %: Empty.type = t._1 %: Empty
  implicit def fromT2[T](t: Tuple2[T, T]): T %: T %: Empty.type = t._1 %: t._2 %: Empty
  implicit def fromT3[T](t: Tuple3[T, T, T]): T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: Empty
  implicit def fromT4[T](t: Tuple4[T, T, T, T]): T %: T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: t._4 %: Empty
  implicit def fromT5[T](t: Tuple5[T, T, T, T, T]): T %: T %: T %: T %: T %: Empty.type = t._1 %: t._2 %: t._3 %: t._4 %: t._5 %: Empty

}

object OrdSet extends ImplicitConversions {
  type ofDim[T, Di <: Dim] = Di#Build[OrdSet[T], R[T]]
  type R[T] = Rec[OrdSet[T]] {
    type Base = Empty.type
    type Succ[X <: OrdSet[T]] = T %: X
  }

  private[OrdSet] def fillUnsafe[T](v: T, number: Int): OrdSet[T] = number match {
    case 0 => Empty
    case n => v %: fillUnsafe(v, n - 1)
  }

  def fill[T, Di <: Dim](v: T)(implicit dv: DimVal[Di]): ofDim[T, Di] =
    fillUnsafe(v, dv.value).asInstanceOf[Di#Build[OrdSet[T], R[T]]]
}