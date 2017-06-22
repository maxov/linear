package linear

import language.higherKinds

sealed trait OrdSet[+T] extends Vector {
  type ThisWith[A] <: OrdSet.ofDim[A, Length]
  type Length <: Dim
  type D = Length
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)
  def map[B](f: T => B): ThisWith[B]
  def mat[R, D <: Dim](implicit ev: T <:< OrdSet.ofDim[R, D]): Mat[R, Length, D] = null
}

case object Empty extends OrdSet[Nothing] {
  type ThisWith[B] = Empty.type
  type Length = Dim._0
  def map[B](f: Nothing => B): ThisWith[B] = this
}

case class %:[+T, +R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type ThisWith[B] = B %: tail.ThisWith[B]
  type Length = Dim.Succ[tail.Length]
  def map[B](f: T => B): ThisWith[B] = f(head) %: tail.map(f)
}

object OrdSet  {
  type ofDim[+T, Di <: Dim] = OrdSet[T] { type Length = Di }
}