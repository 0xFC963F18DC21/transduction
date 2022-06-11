package net.nergi

import scala.annotation.tailrec

package object transduction {
  /** Left-reduce a collection of items, with respect to the reducer's laziness.
    * @param red
    *   Reducer to use.
    * @param state
    *   Current state for the reduction.
    * @param init
    *   Initial value of the reduction.
    * @param coll
    *   Collection to reduce.
    * @tparam S
    *   Type of the reducer's state.
    * @tparam A
    *   Type being reduced by the reducer.
    * @tparam R
    *   Type being reduced into.
    * @return
    *   The final state and reduction result of the item.
    */
  @tailrec
  private def reduceLeft[S, A, R](
    red: Reducer[S, A, R],
    state: S,
    init: R,
    coll: Iterable[A]
  ): (S, R) =
    coll.headOption match {
      case None    => (state, init)
      case Some(x) =>
        red.stepL(state, init, x) match {
          case (newState, Continue(newInit)) => reduceLeft(red, newState, newInit, coll.tail)
          case (newState, Reduced(item))     => (newState, item)
        }
    }

  /** Left-Transduce a collection, given a transducer, reducer, an initial value and the collection
    * to transduce.
    * @param xform
    *   Transducer to use.
    * @param red
    *   Reducer to use.
    * @param init
    *   Initial return value/
    * @param coll
    *   Collection to transduce.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam T
    *   New type of state after transducer transformation.
    * @tparam A
    *   Type of input accepted by the reducer.
    * @tparam B
    *   New type of input accepted by the transformed reduction.
    * @tparam R
    *   Result type of reduction.
    * @return
    *   The final result of the transduction.
    */
  def transduceLeft[S, T, A, B, R](
    xform: Transducer[S, T, A, B],
    red: Reducer[S, A, R],
    init: R,
    coll: Iterable[B]
  ): R = {
    val xformed = xform(red)
    reduceLeft[T, B, R](xformed, xformed.state(), init, coll) match {
      case (state, res) => xformed.completion(state, res)
    }
  }

  /** Right-reduce a collection of items, with respect to the reducer's laziness.
    * @param red
    *   Reducer to use.
    * @param state
    *   Current state for the reduction.
    * @param init
    *   Initial value of the reduction.
    * @param coll
    *   Collection to reduce.
    * @tparam S
    *   Type of the reducer's state.
    * @tparam A
    *   Type being reduced by the reducer.
    * @tparam R
    *   Type being reduced into.
    * @return
    *   The new state and next intermediate item of the reduction.
    */
  private def reduceRight[S, A, R](
    red: Reducer[S, A, R],
    state: S,
    init: R,
    coll: Iterable[A]
  ): (S, Reduction[R]) =
    coll.headOption match {
      case None    => (state, Continue(init))
      case Some(x) =>
        reduceRight(red, state, init, coll.tail) match {
          case (newState, Continue(result)) => red.stepR(newState, x, result)
          case (newState, Reduced(result))  => (newState, Reduced(result))
        }
    }

  /** Right-Transduce a collection, given a transducer, reducer, an initial value and the collection
    * to transduce.
    * @param xform
    *   Transducer to use.
    * @param red
    *   Reducer to use.
    * @param init
    *   Initial return value/
    * @param coll
    *   Collection to transduce.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam T
    *   New type of state after transducer transformation.
    * @tparam A
    *   Type of input accepted by the reducer.
    * @tparam B
    *   New type of input accepted by the transformed reduction.
    * @tparam R
    *   Result type of reduction.
    * @return
    *   The final result of the transduction.
    */
  def transduceRight[S, T, A, B, R](
    xform: Transducer[S, T, A, B],
    red: Reducer[S, A, R],
    init: R,
    coll: Iterable[B]
  ): R = {
    val xformed = xform(red)
    reduceRight[T, B, R](xformed, xformed.state(), init, coll) match {
      case (state, res) => xformed.completion(state, res.item)
    }
  }
}
