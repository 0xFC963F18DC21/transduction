package transduction.mutable

import transduction.{Reducer => ImmutableReducer}

/** Explicitly mutable-state (or stateless) reducers.
  *
  * See [[transduction.Reducer]] for more information.
  *
  * @tparam A
  *   Type of item being reduced.
  * @tparam R
  *   Type of the final result being produced by this reducer.
  */
trait Reducer[-A, R] extends ImmutableReducer[Unit, A, R]
