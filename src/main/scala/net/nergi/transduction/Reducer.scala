package net.nergi.transduction

/** A reducer object.
  *
  * Reducer objects represent three functions that work together to create an applicable object that
  * reduces / "folds" a series of items into a singular value.
  *
  * This particular trait exposes an immutable state getter function. Mutable reducers will most
  * likely always return [[Unit]] for its state.
  *
  * An implementation of a reducer must have at least `stepL` or `stepR` implemented. Optionally, an
  * initial value and completion can be given, but if omitted, they must be provided if used in a
  * transduction process.
  *
  * @tparam S
  *   Type of state used by this reducer.
  * @tparam A
  *   Type of item being reduced.
  * @tparam R
  *   Type of the final result being produced by this reducer.
  */
trait Reducer[S, A, R] {
  /** Get the current immutable state that this reducer is in.
    * @return
    *   Reducer's current state.
    */
  def state(): S

  /** Get this reducer's initial or identity value if defined. Override to define an initial value.
    *
    * This value is used whenever a reduction call does not provide an initial value.
    * @return
    *   Initial value of reduction for this reducer.
    */
  def identity(): R = throw new NoSuchElementException("No initial value given to this reducer.")

  /** This reducer's completion function.
    *
    * The completion function transforms the final result of a reduction. By default, it does not
    * use its state and simply calls the identity.
    *
    * Override to provide your own completion.
    * @param state
    *   Given state of the reducer.
    * @param acc
    *   The final result of the reduction, pre-transformation.
    * @return
    *   The final result of the reduction, post-transformation.
    */
  def completion(state: S, acc: R): R = acc

  /** The left-reduction step of this reducer.
    * @param state
    *   Given state of the reducer.
    * @param acc
    *   The current accumulated value.
    * @param inp
    *   The current value to fold into the accumulated value.
    * @return
    *   An intermediate [[Reduction]] with the new accumulated function.
    */
  def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) =
    stepR(state, inp, acc)

  /** The right-reduction step of this reducer.
    * @param state
    *   Given state of the reducer.
    * @param inp
    *   The current value to fold into the accumulated value.
    * @param acc
    *   The current accumulated value.
    * @return
    *   An intermediate [[Reduction]] with the new accumulated function.
    */
  def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) =
    stepL(state, acc, inp)
}
