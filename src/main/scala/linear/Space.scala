package linear

trait Vector {
  type D <: Dim
}

trait Space { space =>

  implicit def dimV: DimVal[D]

  type D <: Dim

  type V <: Vector

  def zero: V

}

object Space {

  type ofDim[Di <: Dim] = Space { type Di = Dim }

}