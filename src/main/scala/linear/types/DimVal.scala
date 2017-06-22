package linear.types

import linear.types.Dim.{Succ, _0}

/**
 * Defines the integer value of a type-level natural [[D]] so that it may be accessed by implicits
 * @see implicits defined in [[DimVal]] companion
 *
 * @tparam D The type-level natural
 */
trait DimVal[D <: Dim] {

  /**
   * The value of the natural as an integer
   *
   * @return Integer value
   */
  def value: Int

}

object DimVal {

  /**
   * Base case: defines value for [[_0]]
   */
  implicit object Zero extends DimVal[_0] {
    def value = 0
  }

  /**
   * Recursive case: defines value for [[Succ]]
   *
   * @param prev The [[DimVal]] for the previous natural number
   * @tparam D The type of the previous natural number
   * @return The value for the successor natural number
   */
  implicit def Succ[D <: Dim](implicit prev: DimVal[D]): DimVal[Succ[D]] = new DimVal[Succ[D]] {
    override def value: Int = prev.value + 1
  }

}