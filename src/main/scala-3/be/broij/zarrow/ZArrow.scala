/*Copyright 2024 Julien Broi

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.*/

package be.broij.zarrow

import scala.annotation.targetName
import scala.Predef.{identity => id}
import zio._

/**
 * A `ZArrow` describes a mapping from `I` to a computation that requires an `R`
 * and either succeeds with an `O`, fails with an `E`, or dies. In other words,
 * it describes a mapping from `I` to `ZIO[R, E, O]`.
 */
opaque type ZArrow[-I, -R, +E, +O] = I => ZIO[R, E, O]

object ZArrow:

  export zlayer.Ops.*

  /**
   * A `ZArrow` that always maps to a `ZIO` that succeeds with a unit value.
   */
  val unit: ZArrow[Any, Any, Nothing, Unit] =
    _ => ZIO.unit

  /**
   * Returns a `ZArrow` that always map to a `ZIO` that succeeds with the
   * inputs.
   */
  def identity[I]: ZArrow[I, Any, Nothing, I] =
    ZIO.succeed(_)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a dying `ZIO` effect.
   */
  def succeed[I, R: Tag, O](f: I => R => O): ZArrow[I, R, Nothing, O] =
    (in: I) => ZIO.service[R].flatMap(env => ZIO.succeed(f(in)(env)))

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that succeeds with
   * an `O` which is the result of executing `f(in)`. When calling `f`, it will
   * catch any exception and translate it into a dying ZIO effect.
   */
  def succeed[I, O](f: I => O): ZArrow[I, Any, Nothing, O] =
    succeed(in => _ => f(in))

  /**
   * Returns a `ZArrow` that maps any input to a `ZIO` that succeeds with `o`.
   * When evaluating `o`, it will catch any exception and translate it into a
   * dying `ZIO` effect.
   */
  def succeed[O](o: => O): ZArrow[Any, Any, Nothing, O] =
    succeed(_ => _ => o)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R & R1` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a dying `ZIO` effect.
   */
  def fromZIO[I, R, R1: Tag, E, O](f: I => R1 => ZIO[R, E, O]): ZArrow[I, R & R1, E, O] =
    (in: I) => ZIO.service[R1].flatMap(env => ZIO.succeed(f(in)(env)).flatten)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO[R, E, O]` which is
   * the result of executing `f(in)`. When calling `f`, it will catch any
   * exception and translate it into a dying `ZIO` effect.
   */
  def fromZIO[I, R, E, O](f: I => ZIO[R, E, O]): ZArrow[I, R, E, O] =
    (in: I) => ZIO.succeed(f(in)).flatten

  /**
   * Lifts `zio` in a `ZArrow` that maps any input to `zio`. When evaluating
   * `zio`, it will catch any exception and translate it into a dying `ZIO`
   * effect.
   */
  def fromZIO[R, E, O](zio: => ZIO[R, E, O]): ZArrow[Any, R, E, O] =
    _ => ZIO.succeed(zio).flatten

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a failing `ZIO` effect.
   */
  def attempt[I, R: Tag, O](f: I => R => O): ZArrow[I, R, Throwable, O] =
    (in: I) => ZIO.service[R].flatMap(env => ZIO.attempt(f(in)(env)))

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that succeeds with
   * an `O` which is the result of executing `f(in)`. When calling `f`, it will
   * catch any exception and translate it into a failing `ZIO` effect.
   */
  def attempt[I, O](f: I => O): ZArrow[I, Any, Throwable, O] =
    attempt(in => _ => f(in))

  /**
   * Returns a `ZArrow` that maps any input to a `ZIO` that succeeds with `o`.
   * When evaluating `o , it will catch any exception and translate it into a
   * failing `ZIO` effect.
   */
  def attempt[O](o: => O): ZArrow[Any, Any, Throwable, O] =
    attempt(_ => _ => o)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R & R1` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a failing `ZIO` effect.
   */
  def fromZIOAttempt[I, R, R1: Tag, E, O](f: I => R1 => ZIO[R, E, O]): ZArrow[I, R & R1, Throwable | E, O] =
    (in: I) => ZIO.service[R1].flatMap(env => ZIO.attempt(f(in)(env)).flatten)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO[R, E, O]` which is
   * the result of executing `f(in)`. When calling `f`, it will catch any
   * exception and translate it into a failing `ZIO` effect.
   */
  def fromZIOAttempt[I, R, E, O](f: I => ZIO[R, E, O]): ZArrow[I, R, Throwable | E, O] =
    (in: I) => ZIO.attempt(f(in)).flatten

  /**
   * Lifts `zio` in a `ZArrow` that maps any input to `zio`. When evaluating
   * `zio`, it will catch any exception and translate it into a failing `ZIO`
   * effect.
   */
  def fromZIOAttempt[R, E, O](zio: => ZIO[R, E, O]): ZArrow[Any, R, Throwable | E, O] =
    _ => ZIO.attempt(zio).flatten

extension [I, R, E, O](zArrow: ZArrow[I, R, E, O])

  private def zLayerOps = zlayer.Ops(zArrow)
  export zLayerOps.*

  /**
   * Applies this `ZArrow` to the input `in`, returning the `ZIO` that it is
   * mapped to.
   */
  inline def apply(in: I): ZIO[R, E, O] =
    zArrow(in)

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
   * applied, it applies this `ZArrow` to `in` and then applies the given
   * `ZArrow` to `in1`.
   */
  @targetName("<*>")
  def combine[I1, E1, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E | E1, (O, O1)] =
    (in: I, in1: I1) => zArrow(in) <*> another(in1)

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
   * applied, it applies in parallel this `ZArrow` to `in` and the given
   * `ZArrow` to `in1`. If either `ZArrow` fails or dies, then the other side
   * will be interrupted.
   */
  @targetName("<&>")
  def combinePar[I1, E1, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E | E1, (O, O1)] =
    (in: I, in1: I1) => zArrow(in) <&> another(in1)

  /**
   * Returns a `ZArrow` that maps inputs of type `I`. When applied, it applies
   * this `ZArrow` to its input and then applies the given `ZArrow` to the same
   * input.
   */
  def zip[R1, E1, O1](another: ZArrow[I, R1, E1, O1]): ZArrow[I, R & R1, E | E1, (O, O1)] =
    (in: I) => zArrow(in) <*> another(in)

  /**
   * Returns a `ZArrow` that maps inputs of type `I`. When applied, it applies in
   * parallel this `ZArrow` to its input and the given `ZArrow` to the same
   * input. If either `ZArrow` fails or dies, then the other side will be
   * interrupted.
   */
  def zipPar[R1, E1, O1](another: ZArrow[I, R1, E1, O1]): ZArrow[I, R & R1, E | E1, (O, O1)] =
    (in: I) => zArrow(in) <&> another(in)

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in1: I1, in: I)`. When
   * applied, it applies an identity `ZArrow` to `in1` and then applies this
   * `ZArrow` to `in`.
   */
  def first[I1]: ZArrow[(I1, I), R, E, (I1, O)] =
    ZArrow.identity[I1].combine(zArrow)

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
   * applied, it applies this `ZArrow` to `in` and then applies an identity
   * `ZArrow` to `in1`.
   */
  def second[I1]: ZArrow[(I, I1), R, E, (O, I1)] =
    combine(ZArrow.identity[I1])

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * successes are mapped by the given `ZArrow`.
   */
  @targetName(">>>")
  def andThen[R1, E1, O1](another: ZArrow[O, R1, E1, O1]): ZArrow[I, R1 & R, E1 | E, O1] =
    andThen(_.flatMap(another))

  /**
   * Returns a `ZArrow` that applies the given `ZArrow` and whose resulting
   * `ZIO` successes are mapped by this `ZArrow`.
   */
  @targetName("<<<")
  def compose[I1, R1, E1](another: ZArrow[I1, R1, E1, I]): ZArrow[I1, R1 & R, E1 | E, O] =
    another.andThen(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * failures are mapped by the given `ZArrow`. Both successes and failures of
   * the given `ZArrow` are forwarded in the error channel of the resulting
   * `ZIO`.
   */
  def errorAndThen[R1, E1, E2](another: ZArrow[E, R1, E1, E2]): ZArrow[I, R & R1, E1 | E2, O] =
    andThen(_.flatMapError(another(_).fold(id, id)))

  /**
   * Returns a `ZArrow` that applies the given `ZArrow` and whose resulting
   * `ZIO` failures are mapped by this `ZArrow`. Both successes and failures of
   * this `ZArrow` are forwarded in the error channel of the resulting `ZIO`.
   */
  def errorCompose[I1, R1, O1](another: ZArrow[I1, R1, I, O1]): ZArrow[I1, R1 & R, E | O, O1] =
    another.errorAndThen(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * failures are mapped by the given `ZArrow`. Successes of the given `ZArrow`
   * are forwarded in the success channel of the resulting `ZIO`. Failures of
   * the given `ZArrow` are forwarded in the failure channel of the resulting
   * `ZIO`.
   */
  def catchAll[R1, E1, O1](another: ZArrow[E, R1, E1, O1]): ZArrow[I, R & R1, E1, O | O1] =
    andThen(_.catchAll(another))

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * failures and defects are mapped by the given `ZArrow`. Successes of the
   * given `ZArrow` are forwarded in the success channel of the resulting `ZIO`.
   * Failures of the given `ZArrow` are forwarded in the failure channel of the
   * resulting `ZIO`.
   */
  def catchAllCause[R1, E1, O1](another: ZArrow[Cause[E], R1, E1, O1]): ZArrow[I, R & R1, E1, O | O1] =
    andThen(_.catchAllCause(another))

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO` are
   * mapped by the given `f` function. When executing, it will catch any
   * exception thrown by `f` and translate it into a dying `ZIO` effect.
   */
  def mapZIO[R1, E1, O1](f: ZIO[R, E, O] => ZIO[R1, E1, O1]): ZArrow[I, R1, E1, O1] =
    andThen(zio => ZIO.succeed(f(zio)).flatten)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * successes are mapped by the given `f` function. When executing, it will
   * catch any exception thrown by `f` and translate it into a dying `ZIO`
   * effect.
   */
  def map[O1](f: O => O1): ZArrow[I, R, E, O1] =
    ZArrow.succeed(f).compose(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * successes are mapped by the given `f` function. When executing, it will
   * catch any exception thrown by `f` and translate it into a failing `ZIO`
   * effect.
   */
  def mapAttempt[O1](f: O => O1): ZArrow[I, R, Throwable | E, O1] =
    ZArrow.attempt(f).compose(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * failures are mapped by the given `f` function. When executing, it will
   * catch any exception thrown by `f` and translate it into a dying `ZIO`
   * effect.
   */
  def mapError[E1](f: E => E1): ZArrow[I, R, E1, O] =
    ZArrow.succeed(f).errorCompose(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * failures are mapped by the given `f` function. When executing, it will
   * catch any exception thrown by `f` and translate it into a failing `ZIO`
   * effect.
   */
  def mapErrorAttempt[E1](f: E => E1): ZArrow[I, R, Throwable | E1, O] =
    ZArrow.attempt(f).errorCompose(zArrow)

  /**
   * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
   * successes are mapped by the given `fO` function. The resulting `ZIO`
   * failures are themselves mapped by the given `fE` function. When executing,
   * it will catch any exception thrown by `fO` or `fE` and translate it into a
   * dying `ZIO ` effect.
   */
  def mapBoth[O1, E1](fE: E => E1, fO: O => O1): ZArrow[I, R, E1, O1] =
    map(fO).mapError(fE)

  /**
   * Returns a `ZArrow` that applies the given `f` function and then applies
   * this `ZArrow`, feeding the output of the former to the later. When
   * executing, it will catch any exception thrown by `f` and translate it into
   * a dying `ZIO` effect.
   */
  def imap[I1](f: I1 => I): ZArrow[I1, R, E, O] =
    compose(ZArrow.succeed(f))

  /**
   * Returns a `ZArrow` that applies the given `f` function and then applies
   * this `ZArrow`, feeding the output of the former to the later. When
   * executing, it will catch any exception thrown by `f` and translate it into
   * a failing `ZIO` effect.
   */
  def imapAttempt[I1](f: I1 => I): ZArrow[I1, R, Throwable | E, O] =
    compose(ZArrow.attempt(f))

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
   * applied, it applies this `ZArrow` to `in` and the resulting `ZIO` success
   * is mapped by the given `f` function which returns another `ZArrow` which is
   * applied to `in1`. When executing, it will catch any exception thrown by `f`
   * and translate it into a dying `ZIO` effect.
   */
  @targetName(">>=")
  def flatMap[I1, R1, E1, O1](f: O => ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E | E1, O1] =
    (in: I, in1: I1) => zArrow(in).flatMap(f(_)(in1))

  /**
   * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
   * applied, it applies this `ZArrow` to `in` and the resulting `ZIO` failure
   * is mapped by the given `f` function which returns another `ZArrow` which is
   * applied to `in1`. When executing, it will catch any exception thrown by `f`
   * and translate it into a dying `ZIO` effect.
   */
  def flatMapError[I1, R1, E1, E2](f: E => ZArrow[I1, R1, E1, E2]): ZArrow[(I, I1), R & R1, E1 | E2, O] =
    (in: I, in1: I1) => zArrow(in).flatMapError(f(_)(in1).fold(id, id))

  /**
   * Returns a `ZArrow` that maps tuples of shape `((in: I, in1: I1), in2: I2)`.
   * When applied, it applies this `ZArrow` to `in` and the resulting `ZIO`
   * success is mapped by the given `fA` function which returns another `ZArrow`
   * which is applied to `in1`. Any resulting `ZIO` failure is also mapped by
   * the given `fB` function which returns another `ZArrow` which is applied to
   * `in2`. When executing, it will catch any exception thrown by `fA` or `fB`
   * and translate it into a dying `ZIO` effect.
   */
  def flatMapBoth[I1, I2, R1, R2, E1, E2, E3, O1](
    fE: E | E1 => ZArrow[I2, R2, E2, E3],
    fO: O => ZArrow[I1, R1, E1, O1]
  ): ZArrow[((I, I1), I2), R & R1 & R2, E2 | E3, O1] =
    flatMap(fO).flatMapError(fE)

  /**
   * Returns a `ZArrow` that applies the given predicate `f` to the `ZIO`
   * successes this `ZArrow` returns. It will map any success value `v` to
   * `Some(v)` when the predicate evaluates to `true`. Otherwise, it will map
   * `v` to `None`. When executing, it will catch any exception thrown by `f`
   * and translate it into a dying `ZIO` effect.
   */
  def withFilter(f: O => Boolean): ZArrow[I, R, E, Option[O]] =
    map(o => Option.when(f(o))(o))

extension [I, I1, R, E, O](zArrow: ZArrow[(I, I1), R, E, O])
  /**
   * Returns a `ZArrow` that reverts the order of the tuples this `ZArrow` maps
   * from.
   */
  def swapInputs: ZArrow[(I1, I), R, E, O] =
    (in1: I1, in: I) => zArrow(in, in1)

extension [I, R, E, O, O1](zArrow: ZArrow[I, R, E, (O, O1)])
  /**
   * Returns a `ZArrow` that reverts the order of the tuples of the `ZIO`
   * successes this `ZArrow` maps to.
   */
  def swapOutputs: ZArrow[I, R, E, (O1, O)] =
    zArrow.map(_.swap)

  /**
   * Returns a `ZArrow` that keeps the first component of the tuple of the `ZIO`
   * successes this `ZArrow` maps to.
   */
  def filterFirst: ZArrow[I, R, E, O] =
    zArrow.map(_._1)

  /**
   * Returns a `ZArrow` that keeps the second component of the tuple of the
   * `ZIO` successes this `ZArrow` maps to.
   */
  def filterSecond: ZArrow[I, R, E, O1] =
    zArrow.map(_._2)
