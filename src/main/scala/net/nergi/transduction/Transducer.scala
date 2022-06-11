package net.nergi.transduction

/** A transducer transforms reducers into other reducers. These may be stateless or stateful.
  *
  * If creating a mutable transducer or stateless transducer, simply pass the old state type on.
  * Otherwise, please pair the old state with the new state.
  *
  * There are a few laws when creating a transducer:
  *   - The new identity / initial value must call the old reducer's identity / initial signature
  *     once.
  *   - The new completion must call the old reducer's completion exactly once.
  *   - The new step functions can call the old reducer's step as many (or as few) times as it
  *     wants.
  * @tparam S1
  *   Old reducer's state type.
  * @tparam S2
  *   New reducer's state type.
  * @tparam I1
  *   Old reducer's input type.
  * @tparam I2
  *   New reducer's input type.
  */
trait Transducer[S1, S2, I1, I2] {
  /** Transform a reducer using this transducer. Compose several transducers' apply methods to
    * compose transducers.
    * @param rf
    *   Reducer to transform.
    * @tparam R
    *   Resultant type of reduction.
    * @return
    *   Transformed reduction.
    */
  def apply[R](rf: Reducer[S1, I1, R]): Reducer[S2, I2, R]
}
