package linear

import scala.annotation.implicitNotFound

@implicitNotFound("The given spaces ${V} and ${W} are not isomorphic -- perhaps they have different dimensions?")
trait Iso[D <: Dim, V <: Space.ofDim[D], W <: Space.ofDim[D]] { self =>

  def apply(from: V#V): W#V

  def unapply(to: W#V): V#V

  def reverse: Iso[D, W, V] = new Iso[D, W, V] {
    def apply(from: W#V): V#V = self.unapply(from)
    def unapply(to: V#V): W#V = self.apply(to)
  }

  def compose[U <: Space.ofDim[D]](that: Iso[D, U, V]): Iso[D, U, W] = new Iso[D, U, W] {
    def apply(from: U#V): W#V = self.apply(that.apply(from))
    def unapply(to: W#V): U#V = that.unapply(self.unapply(to))
  }

  def andThen[X <: Space.ofDim[D]](that: Iso[D, W, X]): Iso[D, V, X] = that.compose(this)

}

private[linear] trait IsoLaws {

  implicit def reflexive[D <: Dim, V <: Space.ofDim[D]]: Iso[D, V, V] =
    new Iso[D, V, V] {
      def apply(from: V#V): V#V = from
      def unapply(to: V#V): V#V = to
    }

  implicit def symmetric[D <: Dim, V <: Space.ofDim[D], W <: Space.ofDim[D]]
  (implicit iso: Iso[D, V, W]): Iso[D, W, V] = iso.reverse

  implicit def transitive[
    D <: Dim,
    X <: Space.ofDim[D],
    V <: Space.ofDim[D],
    W <: Space.ofDim[D]
  ](implicit iso1: Iso[D, X, V], iso2: Iso[D, V, W]): Iso[D, X, W] =
    iso1 andThen iso2


}

object Iso extends IsoLaws {

  def apply[D <: Dim, V <: Space.ofDim[D], W <: Space.ofDim[D]]
  (implicit iso: Iso[D, V, W]): Iso[D, V, W] = iso

}