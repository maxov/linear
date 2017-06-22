package linear

trait Vector {
  type D <: Dim
}

trait Space { space =>

  type D <: Dim

  type V <: Vector {
    type D <: space.D
  }

  def zero: V

}

object Space {

  type ofDim[Di <: Dim] = Space { type Di = Dim }

}