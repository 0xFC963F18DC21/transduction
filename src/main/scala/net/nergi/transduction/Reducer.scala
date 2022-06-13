package net.nergi.transduction

import net.nergi.transduction.mutable.{Reducer => MutableReducer}

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
  * Any monoids are automatically candidates for being a reducer.
  *
  * @tparam S
  *   Type of state used by this reducer.
  * @tparam A
  *   Type of item being reduced.
  * @tparam R
  *   Type of the final result being produced by this reducer.
  */
trait Reducer[S, -A, R] {
  /** Get the initial immutable state for this reducer.
    * @return
    *   Reducer's initial state.
    */
  def initialState(): S

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

object Reducer {
  /** Create a new stateless reducer.
    * @param id
    *   Identity of reducer.
    * @param comp
    *   Completion of reducer.
    * @param sL
    *   Left-step of reducer. Can be the flipped right-step.
    * @param sR
    *   Right-step of reducer. Can be the flipped left-step.
    * @tparam A
    *   Type of input used by reducer.
    * @tparam R
    *   Type of result of reducer.
    * @return
    *   A stateless reducer from input type to output type, using given step functions.
    */
  def stateless[A, R](
    id: => R,
    comp: R => R,
    sL: (R, A) => Reduction[R],
    sR: (A, R) => Reduction[R]
  ): Reducer[Unit, A, R] =
    new MutableReducer[A, R] {
      override def initialState(): Unit = ()

      override def identity(): R = id

      override def completion(state: Unit, acc: R): R = comp(acc)

      override def stepL(state: Unit, acc: => R, inp: A): (Unit, Reduction[R]) =
        (state, sL(acc, inp))

      override def stepR(state: Unit, inp: A, acc: => R): (Unit, Reduction[R]) =
        (state, sR(inp, acc))
    }

  /** Create a stateless reducer with only a left-step. See [[stateless]] for more information.
    */
  def statelessL[A, R](id: => R, comp: R => R, sL: (R, A) => Reduction[R]): Reducer[Unit, A, R] =
    stateless(id, comp, sL, (a, r) => sL(r, a))

  /** Create a stateless reducer with only a right-step. See [[stateless]] for more information.
    */
  def statelessR[A, R](id: => R, comp: R => R, sR: (A, R) => Reduction[R]): Reducer[Unit, A, R] =
    stateless(id, comp, (r, a) => sR(a, r), sR)

  // Simple boolean reducers.
  /** Boolean-and reducer. */
  def AndReducer(comp: Boolean => Boolean = identity): Reducer[Unit, Boolean, Boolean] =
    statelessR(true, comp, (a, r) => if (!a) Reduced(false) else Continue(a && r))

  /** Boolean-or reducer. */
  def OrReducer(comp: Boolean => Boolean = identity): Reducer[Unit, Boolean, Boolean] =
    statelessR(false, comp, (a, r) => if (a) Reduced(true) else Continue(a || r))

  /** Boolean-exclusive-or reducer. */
  def XorReducer(comp: Boolean => Boolean = identity): Reducer[Unit, Boolean, Boolean] =
    statelessR(false, comp, (a, r) => Continue(a ^ r))

  // Simple arithmetic reducers.
  /** Plus / Add reducer. */
  def AddReducer[@specialized T](comp: T => T = identity _)(implicit
    ev: Numeric[T]
  ): Reducer[Unit, T, T] =
    stateless(ev.zero, comp, (r, a) => Continue(ev.plus(r, a)), (a, r) => Continue(ev.plus(a, r)))

  /** Multiply / Times reducer. */
  def MuliplyReducer[@specialized T](comp: T => T = identity _)(implicit
    ev: Numeric[T]
  ): Reducer[Unit, T, T] =
    stateless(ev.one, comp, (r, a) => Continue(ev.times(r, a)), (a, r) => Continue(ev.times(a, r)))

  // Other reducers.
  /** List-combining reducer. */
  def ListAppendReducer[T](
    comp: List[T] => List[T] = identity[List[T]] _
  ): Reducer[Unit, List[T], List[T]] =
    statelessR(Nil, comp, (a, r) => Continue(a ::: r))

  /** List-cons reducer. */
  def ListConsReducer[T](
    comp: List[T] => List[T] = identity[List[T]] _
  ): Reducer[Unit, T, List[T]] =
    statelessR(Nil, comp, (a, r) => Continue(a :: r))
}
