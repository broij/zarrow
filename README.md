# ZArrow

`ZArrow` is a functional programming library built on top of the [`ZIO`](https://zio.dev) (ZIO: Zero-dependency, type-safe, asynchronous, concurrent) library in Scala. 

The library implements the `ZArrow[I, R, E, O]`. Each instance of `ZArrow` describes an effectful mapping from `I` to `O`. Specifically, it maps any `I` to a `ZIO[R, E, O]`, which is a computation that requires an `R` and either succeeds with an `O`, fails with an `E`, or dies.

While `ZIO` provides features to simplify working with effects, this library aims to further simplify working with effectful mappings. It offers a set of factory methods to lift compatible values into a `ZArrow`, which can then be transformed using a comprehensive set of operators (e.g., `imap`, `map`, `flatMap`, `combine`, etc.).

## Conceptual model

A `ZIO[R, E, A]` can be thought of as a function `R => Either[E, A]` where `R`, called the environment, is passed implicitly (given/implicit parameter).

On the other hand, a `ZArrow[I, R, E, O]` can be thought of as a function `I => R => Either[E, O]` where `I` is passed explicitly and `R` is passed implicitly.

Another way to look at a `ZArrow[I, R, E, O]` is to think about it as a `ZIO[R, E, O]` to which another input channel `I` was added.

## Quick-start

To use `ZArrow` in your Scala project, add the following dependencies to your build.sbt:
```scala
libraryDependencies += "dev.zio" %% "zio" % zioVersion
libraryDependencies += "be.broij" %% "zarrow" % zArrowVersion
```

## Usage

We provide an overview of the utilities this library offer. Each method mentioned in the following sections is fully documented in the source code. For further information, please refer to that documentation.

### Creating a `ZArrow`

One can create a `ZArrow` from scratch using:
- `ZArrow.unit`
- `ZArrow.identity`

### Lifting values into a `ZArrow`

One can lift compatible values into a `ZArrow` using the following factory methods:
- `ZArrow.succeed`
- `ZArrow.attempt`
- `ZArrow.fromZIO`
- `ZArrow.fromZIOAttempt`

### Applying a `ZArrow`

Each `ZArrow` has an `apply` method that allows to compute the `ZIO` a value is mapped to.
There are also some variants of the `apply` method that map collections of values sequentially.
Finally, the `par` methods map collections of values in parallel.

### Transforming a `ZArrow`

One can transform a `ZArrow` using a variety of operators:
- `combine` (or `<*>`)
- `combinePar` (or `<&>`)
- `zip`
- `zipPar`
- `first`
- `second`
- `andThen` (or `>>>`)
- `compose` (or `<<<`)
- `errorAndThen`
- `errorCompose`
- `catchAll`
- `catchAllCause`
- `mapZIO`
- `map`
- `mapAttempt`
- `mapError`
- `mapErrorAttempt`
- `mapBoth`
- `imap`
- `imapAttempt`
- `flatMap` (or `>>=`)
- `flatMapError`
- `flatMapBoth`
- `withFilter`
- `swapInputs`
- `swapOutputs`
- `filterFirst`
- `filterSecond`