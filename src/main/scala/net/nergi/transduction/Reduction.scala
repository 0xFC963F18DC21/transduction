package net.nergi.transduction

/** An intermediate or final representation of a reduction's accumulated value.
  * @tparam T
  *   Type held in this intermediate.
  */
sealed trait Reduction[+T] {
  val item: T
}

/** The reduction has terminated and no longer requires further reduction.
  * @param item
  *   Final result.
  * @tparam T
  *   Type held in this intermediate.
  */
case class Reduced[T](item: T) extends Reduction[T]

/** The reduction has not finished and still requires further reduction.
  * @param item
  *   Intermediate result.
  * @tparam T
  *   Type held in this intermediate.
  */
case class Continue[T](item: T) extends Reduction[T]
