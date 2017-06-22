package linear

sealed trait OrdSet[+T] {
  type Length <: Dim
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)
}

case object Empty extends OrdSet[Nothing] {
  type Length = Dim._0
}

case class %:[+T, R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type Length = Dim.Succ[R#Length]
}

trait LengthProofs {

  trait Concrete[A <: OrdSet[_], B <: OrdSet[_]] {
    def take(a: A): B = _
  }

  /**
    * FAKE!!!
    */
  private[LengthProofs] object Concrete {
    def apply[A <: OrdSet[_], B <: OrdSet[_]] = new Concrete[A, B] {}
  }

  implicit def OIEmpty[T]: Concrete[OrdSet.ofDim[T, Dim._0], Empty.type] = Concrete.apply
  implicit def OIRec[T, R <: OrdSet[T], D <: Dim]
  (implicit that: Concrete[OrdSet.ofDim[T, D], R]): Concrete[OrdSet.ofDim[T, Dim.Succ[D]], T %: R] =
    Concrete.apply

  implicit def finalConversion[A <: OrdSet[_], B <: OrdSet[_]](t: A)(implicit f: Concrete[A, B]): B = f.take(t)

}

object OrdSet extends LengthProofs {
  type ofDim[+T, D <: Dim] = OrdSet[T] { type Length <: D }

  def take2(x: OrdSet.ofDim[Int, Dim._2]): Int = OrdSet.finalConversion(x).head
}