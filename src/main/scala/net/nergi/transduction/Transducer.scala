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
trait Transducer[S1, S2, +I1, -I2] {
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

object Transducer {
  /** Identity transducer. Does not transform a reducer.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam I
    *   Type of input used by reducer.
    */
  case class IdentityTransducer[S, I]() extends Transducer[S, S, I, I] {
    override def apply[R](rf: Reducer[S, I, R]): Reducer[S, I, R] = rf
  }

  /** Mapping transducer. Maps the input using a function before reducing.
    * @param f
    *   Mapping function.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by the mapping function.
    * @tparam B
    *   Type of input taken by reducer.
    */
  case class MappingTransducer[S, A, B](f: A => B) extends Transducer[S, S, B, A] {
    override def apply[R](rf: Reducer[S, B, R]): Reducer[S, A, R] = new Reducer[S, A, R] {
      override def state(): S = rf.state()

      override def identity(): R = rf.identity()

      override def completion(state: S, acc: R): R = rf.completion(state, acc)

      override def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) =
        rf.stepL(state, acc, f(inp))

      override def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) =
        rf.stepR(state, f(inp), acc)
    }
  }

  /** Take transducer. Only accepts the first n items for reduction.
    * @param n
    *   Number of items to accept. Must be >= 0.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class TakeTransducer[S, A](n: Int) extends Transducer[S, (Int, S), A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[(Int, S), A, R] =
      new Reducer[(Int, S), A, R] {
        override def state(): (Int, S) = (n, rf.state())

        override def identity(): R = rf.identity()

        override def completion(state: (Int, S), acc: R): R = rf.completion(state._2, acc)

        override def stepL(state: (Int, S), acc: => R, inp: A): ((Int, S), Reduction[R]) =
          step(state, acc)(s => rf.stepL(s, acc, inp))

        override def stepR(state: (Int, S), inp: A, acc: => R): ((Int, S), Reduction[R]) =
          step(state, acc)(s => rf.stepR(s, inp, acc))

        private def step(state: (Int, S), acc: => R)(
          resLmb: S => (S, Reduction[R])
        ): ((Int, S), Reduction[R]) =
          state match {
            case (x, s) if x > 0 =>
              resLmb(s) match {
                case (ns, na) => ((x - 1, ns), na)
              }
            case _               => (state, Reduced(acc))
          }
      }
  }

  /** Drop transducer. Only accepts items after the first n items for reduction.
    * @param n
    *   Number of items to reject. Must be >= 0.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class DropTransducer[S, A](n: Int) extends Transducer[S, (Int, S), A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[(Int, S), A, R] =
      new Reducer[(Int, S), A, R] {
        override def state(): (Int, S) = (n, rf.state())

        override def identity(): R = rf.identity()

        override def completion(state: (Int, S), acc: R): R = rf.completion(state._2, acc)

        override def stepL(state: (Int, S), acc: => R, inp: A): ((Int, S), Reduction[R]) =
          step(state, acc)(s => rf.stepL(s, acc, inp))

        override def stepR(state: (Int, S), inp: A, acc: => R): ((Int, S), Reduction[R]) =
          step(state, acc)(s => rf.stepR(s, inp, acc))

        private def step(state: (Int, S), acc: => R)(
          resLmb: S => (S, Reduction[R])
        ): ((Int, S), Reduction[R]) =
          state match {
            case (x, s) if x <= 0 =>
              resLmb(s) match {
                case (ns, na) => ((x, ns), na)
              }
            case (x, s)           => ((x - 1, s), Reduced(acc))
          }
      }
  }

  /** To-String transducer. Maps everything into a string.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    * @return
    *   A string conversion transducer.
    */
  def ToStringTransducer[S, A]: Transducer[S, S, String, A] = MappingTransducer(_.toString)

  /** Debug transducer. Adds in debug prints to a transducer.
    * @param name
    *   Prefix of printed message.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    * @return
    *   A debugging transducer that shows messages with the given prefix.
    */
  def DebugTransducer[S, A](name: String = "DEBUG"): Transducer[S, S, A, A] =
    new Transducer[S, S, A, A] {
      override def apply[R](rf: Reducer[S, A, R]): Reducer[S, A, R] = new Reducer[S, A, R] {
        override def state(): S = {
          val state = rf.state()

          printf("%s - CURRENT STATE: %s%n", name, state)
          state
        }

        override def identity(): R = {
          val ident = rf.identity()

          printf("%s - IDENTITY: %s%n", name, ident)
          ident
        }

        override def completion(state: S, acc: R): R = {
          val result = rf.completion(state, acc)

          printf("%s - COMPLETION: %s, %s => %s%n", name, state, acc, result)
          result
        }

        override def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) = {
          val result = rf.stepL(state, acc, inp)

          printf("%s - STEP-LEFT: %s, %s, %s => %s%n", name, state, acc, inp, result)
          result
        }

        override def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) = {
          val result = rf.stepR(state, inp, acc)

          printf("%s - STEP-RIGHT: %s, %s, %s => %s%n", name, state, inp, acc, result)
          result
        }
      }
    }
}
