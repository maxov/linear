package linear

import linear.matrix.Mat
import linear.types.{Dim, DimVal, Rec, TupleImplicitConversions}

import language.higherKinds
import language.implicitConversions
import scala.annotation.unchecked.uncheckedVariance

/**
 * Represents an ordered set of elements of type [[T]]. The set's length is fixed and is part of its type.
 *
 * @tparam T The type of elements contained in the set
 */
sealed trait OrdSet[+T] {

  /**
   * Represents the length of the set as a type.
   */
  type Length <: Dim

  /**
   * Append a new value to the front of this ordered set
   *
   * @param head The new value
   * @tparam V The type of the value (must be at least the type [[T]])
   * @return The ordered set with an appended value
   */
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)

  /**
   *
   * @param f
   * @tparam B
   * @return
   */
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]]

  def mat[R, D <: Dim](implicit ev: T <:< OrdSet.ofDim[R, D]): Mat[R, Length, D] = null

  def values: List[T]

  override def toString = values.mkString("ord{", ", ", "}")

}

case object Empty extends OrdSet[Nothing] {
  type Length = Dim._0
  def values = List()
  def map[B](f: Nothing => B): Empty.type = this
}

case class %:[+T, +R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type Length = Dim.Succ[tail.Length]
  def values: List[T] = head :: tail.values
  def map[B](f: T => B): Length#Build[OrdSet[B], OrdSet.R[B]] =
    (f(head) %: tail.map[B](f)).asInstanceOf[Length#Build[OrdSet[B], OrdSet.R[B]]]
}

object OrdSet {

  /**
   * Defines a [[Rec]] instance for ordered sets.
   *
   * @tparam T The type of the build set
   */
  type R[T] = Rec[OrdSet[T]] {

    /**
     * @inheritdoc
     */
    type Base = Empty.type

    /**
     * @inheritdoc
     */
    type Succ[X <: OrdSet[T]] = T %: X

  }

  /**
   * Represents the type encoding ordered sets of a certain dimension.
   *
   * @tparam T The type contained in ordered sets
   * @tparam Di The dimension of the ordered set
   */
  type ofDim[T, Di <: Dim] = Di#Build[OrdSet[T], R[T]]

  private[OrdSet] def fillUnsafe[T](v: T, number: Int): OrdSet[T] = number match {
    case 0 => Empty
    case n => v %: fillUnsafe(v, n - 1)
  }

  def fill[T, Di <: Dim](v: T)(implicit dv: DimVal[Di]): ofDim[T, Di] =
    fillUnsafe(v, dv.value).asInstanceOf[ofDim[T, Di]]
}