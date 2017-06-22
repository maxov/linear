package linear.types

/**
 * Represents how to recursively build a type [[A]]
 * @see [[linear.OrdSet.R]] for example
 *
 * @tparam A The resultant type
 */
trait Rec[A] {

  /**
   * The base case
   */
  type Base <: A

  /**
   * How to succeed the type [[X]]
   *
   * @tparam X The previous type
   */
  type Succ[X <: A] <: A
}