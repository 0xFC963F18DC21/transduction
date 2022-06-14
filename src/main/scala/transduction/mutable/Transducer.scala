package transduction.mutable

import transduction.{
  reduceLeft1,
  reduceRight1,
  Bias,
  BiasL,
  BiasR,
  Continue,
  Reduced,
  Reducer,
  Reduction,
  Transducer => ImmutableTransducer
}

import scala.collection.mutable

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
    * not reuse singular instances of reducers modified by this transducer!
    * @param n
    *   Number of items to take.
    * @tparam A
    *   Input type of reducer.
    */
  case class TakeMTransducer[A](n: Int) extends Transducer[A, A] {
    override def apply[R](rf: Reducer[Unit, A, R]): Reducer[Unit, A, R] =
      new Reducer[Unit, A, R] {
        private var Count: Int = n

        override def initialState(): Unit = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: Unit, acc: R): R = rf.completion(state, acc)

        override def stepL(state: Unit, acc: => R, inp: A): (Unit, Reduction[R]) =
          if (Count > 0) {
            Count -= 1
            rf.stepL(state, acc, inp)
          } else (state, Reduced(acc))

        override def stepR(state: Unit, inp: A, acc: => R): (Unit, Reduction[R]) =
          if (Count > 0) {
            Count -= 1
            rf.stepR(state, inp, acc)
          } else (state, Reduced(acc))
      }
  }

  /** Mutable drop transducer. Uses a mutable inner state to figure out how many items to reject. Do
    * not reuse singular instances of reducers modified by this transducer!
    * @param n
    *   Number of items to take.
    * @tparam A
    *   Input type of reducer.
    */
  case class DropMTransducer[A](n: Int) extends Transducer[A, A] {
    override def apply[R](rf: Reducer[Unit, A, R]): Reducer[Unit, A, R] =
      new Reducer[Unit, A, R] {
        private var Count: Int = n

        override def initialState(): Unit = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: Unit, acc: R): R = rf.completion(state, acc)

        override def stepL(state: Unit, acc: => R, inp: A): (Unit, Reduction[R]) =
          if (Count > 0) {
            Count -= 1
            (state, Continue(acc))
          } else
            rf.stepL(state, acc, inp)

        override def stepR(state: Unit, inp: A, acc: => R): (Unit, Reduction[R]) =
          if (Count > 0) {
            Count -= 1
            (state, Continue(acc))
          } else
            rf.stepR(state, inp, acc)
      }
  }

  /** Mutable categorising transducer. See [[transduction.Transducer.CategorisingTransducer]] for
    * more information.
    *
    * Although this transducer can be re-used, do not share one instance of any transformed reducers
    * across different threads!
    * @param categoriser
    *   Key-generating function for the internal categoriser.
    * @tparam K
    *   Desired categorising type.
    * @tparam A
    *   Input type of reducer.
    */
  case class CategorisingMTransducer[K, A](categoriser: A => K, bias: Bias = BiasL)
    extends Transducer[List[A], A] {
    override def apply[R](rf: Reducer[Unit, List[A], R]): Reducer[Unit, A, R] = {
      new Reducer[Unit, A, R] {
        private val Partitions: mutable.Map[K, List[A]] = new mutable.LinkedHashMap()

        override def initialState(): Unit = rf.initialState()

        override def identity(): R = rf.identity()

        override def completion(state: Unit, acc: R): R = {
          val lists = Partitions.map { case (_, l) => l.reverse }.toList
          Partitions.clear()

          bias match {
            case BiasL =>
              reduceLeft1[Unit, List[A], R](rf, lists)
            case BiasR =>
              reduceRight1[Unit, List[A], R](rf, lists)
          }
        }

        override def stepL(state: Unit, acc: => R, inp: A): (Unit, Reduction[R]) = {
          Partitions.updateWith(categoriser(inp)) {
            case None    => Some(List(inp))
            case Some(l) => Some(inp :: l)
          }

          ((), Continue(acc))
        }
      }
    }
  }
}
