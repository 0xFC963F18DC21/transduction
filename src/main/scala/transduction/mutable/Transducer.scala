package transduction.mutable

import transduction.{Reduced, Reducer, Reduction, Transducer => ImmutableTransducer}

/** Explicitly mutable-state (or stateless) transducers. These are more flexible in their typing,
  * but cannot be reused freely due to their mutable state.
  *
  * However, these can only modify stateless or mutable-state transducers.
  *
  * See [[transduction.Transducer]] for more information.
  *
  * @tparam I1
  *   Old reducer's input type.
  * @tparam I2
  *   New reducer's input type.
  */
trait Transducer[I1, I2] extends transduction.Transducer[Unit, Unit, I1, I2] {}

object Transducer {
  /** Mapping transducer. Adapted to fit mutable transduction.
    *
    * See [[transduction.Transducer.MappingTransducer]] for more information.
    * @param f
    *   Mapping function (ideally pure).
    * @tparam A
    *   New input type to process.
    * @tparam B
    *   Old input type being processed.
    */
  case class MappingMTransducer[A, B](f: A => B) extends Transducer[B, A] {
    private val ImmT = ImmutableTransducer.MappingTransducer[Unit, A, B](f)

    override def apply[R](rf: Reducer[Unit, B, R]): Reducer[Unit, A, R] = ImmT(rf)
  }

  /** Filtering transducer. Adapted to fit mutable transduction.
    *
    * See [[transduction.Transducer.FilteringTransducer]] for more information.
    * @param p
    *   Predicate function (ideally pure).
    * @tparam A
    *   Input type of reducer.
    */
  case class FilteringMTransducer[A](p: A => Boolean) extends Transducer[A, A] {
    private val ImmT = ImmutableTransducer.FilteringTransducer[Unit, A](p)

    override def apply[R](rf: Reducer[Unit, A, R]): Reducer[Unit, A, R] = ImmT(rf)
  }

  /** Mutable take transducer. Uses a mutable inner state to figure out how many items to accept. Do
    * not reuse singular instances of this transducer!
    * @param n
    *   Number of items to take.
    * @tparam A
    *   Input type of reducer.
    */
  case class TakeMTransducer[A](private var n: Int) extends Transducer[A, A] {
    override def apply[R](rf: Reducer[Unit, A, R]): Reducer[Unit, A, R] =
      new Reducer[Unit, A, R] {
        override def initialState(): Unit = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: Unit, acc: R): R = rf.completion(state, acc)

        override def stepL(state: Unit, acc: => R, inp: A): (Unit, Reduction[R]) =
          if (n > 0) {
            n -= 1
            rf.stepL(state, acc, inp)
          } else (state, Reduced(acc))

        override def stepR(state: Unit, inp: A, acc: => R): (Unit, Reduction[R]) =
          if (n > 0) {
            n -= 1
            rf.stepR(state, inp, acc)
          } else (state, Reduced(acc))
      }
  }
}
