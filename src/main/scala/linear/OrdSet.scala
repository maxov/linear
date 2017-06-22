package linear

sealed trait OrdSet[+T] {
  type Length <: Dim
  def %:[V >: T](head: V): %:[V, this.type] = new %:(head, this)
}

case object Empty extends OrdSet[Nothing] {
  type Length = Dim._0
}

case class %:[+T, +R <: OrdSet[T]](head: T, tail: R) extends OrdSet[T] {
  type Length = Dim.Succ[tail.Length]
}

trait LengthProofs {

  case class Concrete[A, B]()

  implicit def OIEmpty[T]: Concrete[OrdSet.ofDim[T, Dim._0], Empty.type] = Concrete()
  //implicit def one[T]: Concrete[OrdSet.ofDim[T, Dim._1], T %: Empty.type] = OIRec
  implicit def OIRec[T, R <: OrdSet[T], D <: Dim](implicit that: Concrete[OrdSet.ofDim[T, D], R]
                                                 ): Concrete[OrdSet.ofDim[T, Dim.Succ[D]], T %: R] =
    Concrete()

  implicit def finalConversion[A, B](t: A)(implicit f: Concrete[A, B]): B = t.asInstanceOf[B]

}

object OrdSet extends LengthProofs {
  type ofDim[+T, D <: Dim] = OrdSet[T] { type Length = D }

  val x: ofDim[Int, Dim._2] = 2 %: 3 %: Empty

  implicitly[Concrete[OrdSet.ofDim[Int, linear.Dim._0], _]]



  implicitly[Concrete[OrdSet.ofDim[Int, linear.Dim._1], _]]

  implicitly[Concrete[ofDim[Int,Dim._2],_]]

  //def take2(x: OrdSet.ofDim[Int, Dim._2]): Int = OrdSet.finalConversion(x).head
}