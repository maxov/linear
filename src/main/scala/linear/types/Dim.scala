package linear.types

/**
 * Represents a type-level natural number.
 */
sealed trait Dim {

  /**
   * Defines at the type-level, the type resulting from applying [[R]] this number of times.
   *
   * @see [[Rec]]
   *
   * @tparam A The resulting type
   * @tparam R A [[Rec]] representing how [[A]] can be repeatedly constructing
   */
  type Build[A, R <: Rec[A]] <: A

}

object Dim {

  private[Dim] final case object _0 extends Dim {
    type Build[A, R <: Rec[A]] = R#Base
  }
  type _0 = _0.type
  final class Succ[T <: Dim] extends Dim {
    type Build[A, R <: Rec[A]] = R#Succ[T#Build[A,R]]
  }

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
  type _6 = Succ[_5]
  type _7 = Succ[_6]
  type _8 = Succ[_7]
  type _9 = Succ[_8]
  type _10 = Succ[_9]

  /**
   * Get the integer value of the given type-level natural
   *
   * @param v The implicit evidence for [[D]]'s integer value
   * @tparam D The type-level natural
   * @return An integer representing the value of
   */
  def v[D <: Dim](implicit v: DimVal[D]): Int = v.value

}