package linear

trait Vector {
  type D <: Dim
}

class Space { space =>

  type D <: Dim

  type V <: Vector {
    type D <: space.D
  }

  type VSet

}

object Space {

  type ofDim[Di <: Dim] = Space { type Di = Dim }

}