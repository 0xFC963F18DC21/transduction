package transduction

import scala.collection.immutable.ListMap

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
  *
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

  /** Composition operator for transducers.
    *
    * Note that the arrow follows the direction of data.
    * @param xf
    *   Transducer to compose with.
    * @tparam S3
    *   State type of next transducer.
    * @tparam I3
    *   Input type of next transducer.
    * @return
    *   Composed transducer.
    */
  def >-:[S3, I3](xf: Transducer[S2, S3, I2, I3]): Transducer[S1, S3, I1, I3] = {
    val old = this
    new Transducer[S1, S3, I1, I3] {
      override def apply[R](rf: Reducer[S1, I1, R]): Reducer[S3, I3, R] = xf(old(rf))
    }
  }

  /** Alias for [[>-:]]. */
  def andThen[S3, I3](xf: Transducer[S2, S3, I2, I3]): Transducer[S1, S3, I1, I3] = xf >-: this

  /** And-Then operator for transducers.
    *
    * Note that the arrow follows the direction of data.
    * @param xf
    *   Transducer to execute next.
    * @tparam S3
    *   State type of next transducer.
    * @tparam I3
    *   Input type of next transducer.
    * @return
    *   Composed transducer.
    */
  def :-<[S3, I3](xf: Transducer[S2, S3, I2, I3]): Transducer[S1, S3, I1, I3] = xf >-: this

  /** Alias for [[:-<]]. */
  def compose[S3, I3](xf: Transducer[S2, S3, I2, I3]): Transducer[S1, S3, I1, I3] = this :-< xf
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
      override def initialState(): S = rf.initialState()

      override def identity(): R = rf.identity()

      override def completion(state: S, acc: R): R = rf.completion(state, acc)

      override def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) =
        rf.stepL(state, acc, f(inp))

      override def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) =
        rf.stepR(state, f(inp), acc)
    }
  }

  /** Filtering transducer. Only accepts inputs which match the predicate.
    * @param p
    *   Predicate used to filter inputs.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input used by reducer.
    */
  case class FilteringTransducer[S, A](p: A => Boolean) extends Transducer[S, S, A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[S, A, R] =
      new Reducer[S, A, R] {
        override def initialState(): S = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: S, acc: R): R = rf.completion(state, acc)

        override def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) =
          if (p(inp)) rf.stepL(state, acc, inp)
          else (state, Continue(acc))

        override def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) =
          if (p(inp)) rf.stepR(state, inp, acc)
          else (state, Continue(acc))
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
        override def initialState(): (Int, S) = (n, rf.initialState())

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

  /** Take-while transducer. Only accepts inputs until the first input that does not match the given
    * predicate function.
    *
    * Ideally, the predicate is pure.
    * @param p
    *   Predicate function for deciding what inputs go in.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class TakeWhileTransducer[S, A](p: A => Boolean) extends Transducer[S, S, A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[S, A, R] =
      new Reducer[S, A, R] {
        override def initialState(): S = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: S, acc: R): R = rf.completion(state, acc)

        override def stepL(state: S, acc: => R, inp: A): (S, Reduction[R]) =
          if (!p(inp)) (state, Reduced(acc))
          else rf.stepL(state, acc, inp)

        override def stepR(state: S, inp: A, acc: => R): (S, Reduction[R]) =
          if (!p(inp)) (state, Reduced(acc))
          else rf.stepR(state, inp, acc)
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
        override def initialState(): (Int, S) = (n, rf.initialState())

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
            case (x, s)           => ((x - 1, s), Continue(acc))
          }
      }
  }

  /** Drop-while transducer. Does not accept inputs until the first input that does not match the
    * given predicate function.
    *
    * Ideally, the predicate is pure.
    * @param p
    *   Predicate function for deciding what inputs do not go in.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class DropWhileTransducer[S, A](p: A => Boolean) extends Transducer[S, (Boolean, S), A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[(Boolean, S), A, R] =
      new Reducer[(Boolean, S), A, R] {
        override def initialState(): (Boolean, S) = (true, rf.initialState())

        override def identity(): R = rf.identity()

        override def completion(state: (Boolean, S), acc: R): R = rf.completion(state._2, acc)

        override def stepL(state: (Boolean, S), acc: => R, inp: A): ((Boolean, S), Reduction[R]) =
          if (state._1 && p(inp)) (state, Continue(acc))
          else
            rf.stepL(state._2, acc, inp) match {
              case (ns, na) => ((false, ns), na)
            }

        override def stepR(state: (Boolean, S), inp: A, acc: => R): ((Boolean, S), Reduction[R]) =
          if (state._1 && p(inp)) (state, Continue(acc))
          else
            rf.stepR(state._2, inp, acc) match {
              case (ns, na) => ((false, ns), na)
            }
      }
  }

  /** Dedupe transducer. Removes all duplicate inputs from the reduction process. It will always
    * leave one of a duplicate set of inputs.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class DedupeTransducer[S, A]() extends Transducer[S, (Set[A], S), A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[(Set[A], S), A, R] =
      new Reducer[(Set[A], S), A, R] {
        override def initialState(): (Set[A], S) = (Set.empty, rf.initialState())

        override def identity(): R = rf.identity()

        override def completion(state: (Set[A], S), acc: R): R = rf.completion(state._2, acc)

        override def stepL(state: (Set[A], S), acc: => R, inp: A): ((Set[A], S), Reduction[R]) =
          if (!state._1.contains(inp)) (state, Continue(acc))
          else
            rf.stepL(state._2, acc, inp) match {
              case (ns, na) => ((state._1 + inp, ns), na)
            }

        override def stepR(state: (Set[A], S), inp: A, acc: => R): ((Set[A], S), Reduction[R]) =
          if (!state._1.contains(inp)) (state, Continue(acc))
          else
            rf.stepR(state._2, inp, acc) match {
              case (ns, na) => ((state._1 + inp, ns), na)
            }
      }
  }

  /** Categorising transducer. Categorises all items into sublists where the categoriser function
    * returns the same value. Value order from iterable is preserved in each sublist.
    * @param categoriser
    *   Key / category generator.
    * @param bias
    *   Which direction to bias the final reduction.
    * @tparam S
    *   Type of state used by reducer.
    * @tparam K
    *   Type of keys / categories used internally.
    * @tparam A
    *   Type of input taken by reducer.
    */
  case class CategorisingTransducer[S, K, A](categoriser: A => K, bias: Bias = BiasL)
    extends Transducer[S, (Map[K, List[A]], S), List[A], A] {
    override def apply[R](rf: Reducer[S, List[A], R]): Reducer[(Map[K, List[A]], S), A, R] =
      new Reducer[(Map[K, List[A]], S), A, R] {
        override def initialState(): (Map[K, List[A]], S) = (ListMap.empty, rf.initialState())

        override def identity(): R = rf.identity()

        override def completion(state: (Map[K, List[A]], S), acc: R): R = {
          val lists = state._1.map { case (_, l) => if (bias == BiasL) l.reverse else l }.toList

          bias match {
            case BiasL =>
              reduceLeft1[S, List[A], R](rf, lists)
            case BiasR =>
              reduceRight1[S, List[A], R](rf, lists.reverse)
          }
        }

        override def stepL(
          state: (Map[K, List[A]], S),
          acc: => R,
          inp: A
        ): ((Map[K, List[A]], S), Reduction[R]) = state match {
          case (m, s) =>
            (
              (
                m.updatedWith(categoriser(inp)) {
                  case None    => Some(List(inp))
                  case Some(l) => Some(inp :: l)
                },
                s
              ),
              Continue(acc)
            )
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
  def ToStringTransducer[S, A](): Transducer[S, S, String, A] = MappingTransducer(_.toString)

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
  case class DebugTransducer[S, A](name: String = "DEBUG") extends Transducer[S, S, A, A] {
    override def apply[R](rf: Reducer[S, A, R]): Reducer[S, A, R] = new Reducer[S, A, R] {
      override def initialState(): S = {
        val state = rf.initialState()

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
