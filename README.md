# Transduction

Clojure's [transducers](https://clojure.org/reference/transducers) in Scala.

Inspired by Clojure's implementation (obviously) and [this Haskell
implementation and theory](https://hypirion.com/musings/haskell-transducers).

Documentation can be found [here](https://0xfc963f18dc21.github.io/transduction/).

## What are they?

Transducers are applicable objects that transform reducers. If you've ever
worked with folding or reduction in a functional language, a reducer is
simply a representation of the reduction function used in those functions.

A reducer has three effective signatures:

- **Identity**: The identity of the reducer. Given when no initial value is used
  for the reduction. E.g. for integer reducers, 0 is the default identity
  for addition reductions, and 1 is the default identity for multiplication
  reductions. (`identity: R`)
- **Completion**: The "finaliser" of a reducer. Called upon the result of the
  reduction as a final transformation. (`completion: R => R`)
- **Step**: The actual reduction part of a reducer. Called with the current
  accumulated reduction and the current item to be reduced. (`stepL: R => A => R`,
  `stepR: A => R => R`)

A transducer adds a layer of transformation onto these reducers, and can be both
stateful or stateless. They are also composeable using simple function composition.

This library aims to provide interfaces for creating immutable and mutable state
transducers and the ability to use them.
