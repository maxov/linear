package linear

import linear.types.Rec

import language.higherKinds

sealed trait OrdSet[+T] extends Vector {
  type Length <: Dim
  type D = Length
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]]
  def mat[R, D <: Dim](implicit ev: T <:< OrdSet.ofDim[R, D]): Mat[R, Length, D] = null
}

case object Empty extends OrdSet[Nothing] {
  type Length = Dim._0
  def map[B](f: Nothing => B): Empty.type = this
}

case class %:[+T, +R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type Length = Dim.Succ[tail.Length]
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]] = (f(head) %: tail.map[B](f)).asInstanceOf[Length#Build[OrdSet[B], OrdSet.R[B]]]
}

object OrdSet  {
  type ofDim[+T, Di <: Dim] = OrdSet[T] { type Length = Di }
  type R[T] = Rec[OrdSet[T]] {
    type Base = Empty.type
    type Succ[X <: OrdSet[T]] = T %: X
  }

  private[OrdSet] def fillUnsafe[T](v: T, number: Int): OrdSet[T] = number match {
    case 0 => Empty
    case n => v %: fillUnsafe(v, n - 1)
  }

  def fill[T, Di <: Dim](v: T)(implicit dv: DimVal[Di]): Di#Build[OrdSet[T], R[T]] =
    fillUnsafe(v, dv.value).asInstanceOf[Di#Build[OrdSet[T], R[T]]]
}