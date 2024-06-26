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

package be.broij

import scala.collection.immutable._
import scala.language.higherKinds
import scala.Predef.{identity => id}
import scala.reflect.ClassTag
import zio._

/**
 * A `ZArrow` describes a mapping from `I` to a computation that requires an `R`
 * and either succeeds with an `O`, fails with an `E`, or dies. In other words,
 * it describes a mapping from `I` to `ZIO[R, E, O]`.
 */
final case class ZArrow[-I, -R, +E, +O] private (f: I => ZIO[R, E, O]) extends AnyVal

object ZArrow {

  /**
   * A `ZArrow` that always maps to a `ZIO` that succeeds with a unit value.
   */
  val unit: ZArrow[Any, Any, Nothing, Unit] =
    ZArrow(_ => ZIO.unit)

  /**
   * Returns a `ZArrow` that always map to a `ZIO` that succeeds with the
   * inputs.
   */
  def identity[I]: ZArrow[I, Any, Nothing, I] =
    ZArrow((in: I) => ZIO.succeed(in))

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a dying `ZIO` effect.
   */
  def succeed[I, R: Tag, O](f: I => R => O): ZArrow[I, R, Nothing, O] =
    ZArrow((in: I) => ZIO.service[R].flatMap(env => ZIO.succeed(f(in)(env))))

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
    ZArrow((in: I) => ZIO.service[R1].flatMap(env => ZIO.succeed(f(in)(env)).flatten))

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO[R, E, O]` which is
   * the result of executing `f(in)`. When calling `f`, it will catch any
   * exception and translate it into a dying `ZIO` effect.
   */
  def fromZIO[I, R, E, O](f: I => ZIO[R, E, O]): ZArrow[I, R, E, O] =
    ZArrow((in: I) => ZIO.succeed(f(in)).flatten)

  /**
   * Lifts `zio` in a `ZArrow` that maps any input to `zio`. When evaluating
   * `zio`, it will catch any exception and translate it into a dying `ZIO`
   * effect.
   */
  def fromZIO[R, E, O](zio: => ZIO[R, E, O]): ZArrow[Any, R, E, O] =
    ZArrow(_ => ZIO.succeed(zio).flatten)

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO` that requires an
   * `r: R` and succeeds with an `O` which is the result of executing
   * `f(in)(r)`. When calling `f`, it will catch any exception and translate it
   * into a failing `ZIO` effect.
   */
  def attempt[I, R: Tag, O](f: I => R => O): ZArrow[I, R, Throwable, O] =
    ZArrow((in: I) => ZIO.service[R].flatMap(env => ZIO.attempt(f(in)(env))))

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
  def fromZIOAttempt[I, R, R1: Tag, E <: Throwable, O](f: I => R1 => ZIO[R, E, O]): ZArrow[I, R & R1, Throwable, O] =
    ZArrow((in: I) => ZIO.service[R1].flatMap(env => ZIO.attempt(f(in)(env)).flatten))

  /**
   * Lifts `f` in a `ZArrow` that maps any `in: I` to a `ZIO[R, E, O]` which is
   * the result of executing `f(in)`. When calling `f`, it will catch any
   * exception and translate it into a failing `ZIO` effect.
   */
  def fromZIOAttempt[I, R, E <: Throwable, O](f: I => ZIO[R, E, O]): ZArrow[I, R, Throwable, O] =
    ZArrow((in: I) => ZIO.attempt(f(in)).flatten)

  /**
   * Lifts `zio` in a `ZArrow` that maps any input to `zio`. When evaluating
   * `zio`, it will catch any exception and translate it into a failing `ZIO`
   * effect.
   */
  def fromZIOAttempt[R, E <: Throwable, O](zio: => ZIO[R, E, O]): ZArrow[Any, R, Throwable, O] =
    ZArrow(_ => ZIO.attempt(zio).flatten)

  implicit class ZArrowOps[I, R, E, O](zArrow: ZArrow[I, R, E, O]) {

    /**
     * Applies this `ZArrow` to the input `in`, returning the `ZIO` that it is
     * mapped to.
     */
    def apply(in: I): ZIO[R, E, O] =
      zArrow.f(in)

    /**
     * Applies this `ZArrow` to each element of the `Set[I]` and returns the
     * results in a new `Set[O]`.
     */
    def apply(set: Set[I]): ZIO[R, E, Set[O]] =
      ZIO.foreach(set)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `Set[I]` in parallel and
     * returns the results in a new `Set[O]`.
     */
    def par(set: Set[I]): ZIO[R, E, Set[O]] =
      ZIO.foreachPar(set)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `Collection[I]` and returns
     * the results in a new `Collection[O]`.
     */
    def apply[Collection[+Element] <: Iterable[Element]](it: Collection[I])(implicit
      buildFrom: BuildFrom[Collection[I], O, Collection[O]]
    ): ZIO[R, E, Collection[O]] =
      ZIO.foreach(it)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `Collection[I]` in parallel
     * and returns the results in a new `Collection[O]`.
     */
    def par[Collection[+Element] <: Iterable[Element]](it: Collection[I])(implicit
      buildFrom: BuildFrom[Collection[I], O, Collection[O]]
    ): ZIO[R, E, Collection[O]] =
      ZIO.foreachPar(it)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `Array[I]` and returns the
     * results in a new `Array[O]`.
     */
    def apply(array: Array[I])(implicit classTag: ClassTag[O]): ZIO[R, E, Array[O]] =
      ZIO.foreach(array)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `Array[I]` in parallel and
     * returns the results in a new `Array[O]`.
     */
    def par(array: Array[I])(implicit classTag: ClassTag[O]): ZIO[R, E, Array[O]] =
      ZIO.foreachPar(array)(zArrow.f)

    /**
     * Applies this `ZArrow`to each element of the `NonEmptyChunk[I]` and
     * returns the results in a new `NonEmptyChunk[O]`.
     */
    def apply(nonEmptyChunk: NonEmptyChunk[I]): ZIO[R, E, NonEmptyChunk[O]] =
      ZIO.foreach(nonEmptyChunk)(zArrow.f)

    /**
     * Applies this `ZArrow` to each element of the `NonEmptyChunk[I]` in
     * parallel and returns the results in a new `NonEmptyChunk[O]`.
     */
    def par(nonEmptyChunk: NonEmptyChunk[I]): ZIO[R, E, NonEmptyChunk[O]] =
      ZIO.foreachPar(nonEmptyChunk)(zArrow.f)

    /**
     * Applies this `ZArrow` if the argument is non-empty and returns the
     * results in a new `Option[O]`
     */
    def apply(option: Option[I]): ZIO[R, E, Option[O]] =
      ZIO.foreach(option)(zArrow.f)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies this `ZArrow` to `in` and then applies the given
     * `ZArrow` to `in1`.
     */
    def combine[I1, E1 >: E, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, (O, O1)] =
      ZArrow(((in: I, in1: I1) => zArrow(in) <*> another(in1)).tupled)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies this `ZArrow` to `in` and then applies the given
     * `ZArrow` to `in1`.
     */
    def <*>[I1, E1 >: E, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, (O, O1)] =
      combine(another)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies in parallel this `ZArrow` to `in` and the given
     * `ZArrow` to `in1`. If either `ZArrow` fails or dies, then the other side
     * will be interrupted.
     */
    def combinePar[I1, E1 >: E, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, (O, O1)] =
      ZArrow(((in: I, in1: I1) => zArrow(in) <&> another(in1)).tupled)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies in parallel this `ZArrow` to `in` and the given
     * `ZArrow` to `in1`. If either `ZArrow` fails or dies, then the other side
     * will be interrupted.
     */
    def <&>[I1, E1 >: E, R1, O1](another: ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, (O, O1)] =
      combinePar(another)

    /**
     * Returns a `ZArrow` that maps inputs of type `I`. When appied, it applies
     * this `ZArrow` to its input and then applies the given `ZArrow` to the
     * same input.
     */
    def zip[R1, E1 >: E, O1](another: ZArrow[I, R1, E1, O1]): ZArrow[I, R & R1, E1, (O, O1)] =
      ZArrow((in: I) => zArrow(in) <*> another(in))

    /**
     * Returns a `ZArrow` that maps inputs of type `I`. When appied, it applies
     * in parallel this `ZArrow` to its input and the given `ZArrow` to the same
     * input. If either `ZArrow` fails or dies, then the other side will be
     * interrupted.
     */
    def zipPar[R1, E1 >: E, O1](another: ZArrow[I, R1, E1, O1]): ZArrow[I, R & R1, E1, (O, O1)] =
      ZArrow((in: I) => zArrow(in) <&> another(in))

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
     * are mapped by the given `f` function. When executing, it will catch any
     * exception thrown by `f` and translate it into a dying `ZIO` effect.
     */
    def mapZIO[R1, E1, O1](f: ZIO[R, E, O] => ZIO[R1, E1, O1]): ZArrow[I, R1, E1, O1] =
      ZArrow(zArrow.f.andThen(zio => ZIO.succeed(f(zio)).flatten))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * successes are mapped by the given `ZArrow`.
     */
    def andThen[R1, E1 >: E, O1](another: ZArrow[O, R1, E1, O1]): ZArrow[I, R1 & R, E1, O1] =
      mapZIO(_.flatMap(another(_)))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * successes are mapped by the given `ZArrow`.
     */
    def >>>[R1, E1 >: E, O1](another: ZArrow[O, R1, E1, O1]): ZArrow[I, R1 & R, E1, O1] =
      andThen(another)

    /**
     * Returns a `ZArrow` that applies the given `ZArrow` and whose resulting
     * `ZIO` successes are mapped by this `ZArrow`.
     */
    def compose[I1, R1, E1 <: E](another: ZArrow[I1, R1, E1, I]): ZArrow[I1, R1 & R, E, O] =
      another.andThen[R, E, O](zArrow)

    /**
     * Returns a `ZArrow` that applies the given `ZArrow` and whose resulting
     * `ZIO` successes are mapped by this `ZArrow`.
     */
    def <<<[I1, R1, E1 <: E](another: ZArrow[I1, R1, E1, I]): ZArrow[I1, R1 & R, E, O] =
      compose(another)

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * failures are mapped by the given `ZArrow`. Both successes and failures of
     * the given `ZArrow` are forwarded in the error channel of the resulting
     * `ZIO`.
     */
    def errorAndThen[R1, E1](another: ZArrow[E, R1, E1, E1]): ZArrow[I, R & R1, E1, O] =
      mapZIO(_.flatMapError(another(_).fold(id, id)))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * failures are mapped by the given `ZArrow`. Successes of the given
     * `ZArrow` are forwarded in the success channel of the resulting `ZIO`.
     * Failures of the given `ZArrow` are forwarded in the failure channel of
     * the resulting `ZIO`.
     */
    def catchAll[R1, E1, O1 >: O](another: ZArrow[E, R1, E1, O1]): ZArrow[I, R & R1, E1, O1] =
      mapZIO(_.catchAll(another(_)))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * failures and defects are mapped by the given `ZArrow`. Successes of the
     * given `ZArrow` are forwarded in the success channel of the resulting
     * `ZIO`. Failures of the given `ZArrow` are forwarded in the failure
     * channel of the resulting `ZIO`.
     */
    def catchAllCause[R1, E1, O1 >: O](another: ZArrow[Cause[E], R1, E1, O1]): ZArrow[I, R & R1, E1, O1] =
      mapZIO(_.catchAllCause(another(_)))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * successes are mapped by the given `f` function. When executing, it will
     * catch any exception thrown by `f` and translate it into a dying `ZIO`
     * effect.
     */
    def map[O1](f: O => O1): ZArrow[I, R, E, O1] =
      andThen(ZArrow.succeed(f))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * failures are mapped by the given `f` function. When executing, it will
     * catch any exception thrown by `f` and translate it into a dying `ZIO`
     * effect.
     */
    def mapError[E1](f: E => E1): ZArrow[I, R, E1, O] =
      errorAndThen(ZArrow.succeed(f))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * successes are mapped by the given `fO` function. The resulting `ZIO`
     * failures are themselves mapped by the given `fE` function. When
     * executing, it will catch any exception thrown by `fO` or `fE` and
     * translate it into a dying `ZIO ` effect.
     */
    def mapBoth[O1, E1](fE: E => E1, fO: O => O1): ZArrow[I, R, E1, O1] =
      map(fO).mapError(fE)

    /**
     * Returns a `ZArrow` that applies the given `f` function and then applies
     * this `ZArrow`, feeding the output of the former to the later. When
     * executing, it will catch any exception thrown by `f` and translate it
     * into a dying `ZIO` effect.
     */
    def imap[I1](f: I1 => I): ZArrow[I1, R, E, O] =
      compose(ZArrow.succeed(f))

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies this `ZArrow` to `in` and the resulting `ZIO` success
     * is mapped by the given `f` function which returns another `ZArrow` which
     * is applied to `in1`. When executing, it will catch any exception thrown
     * by `f` and translate it into a dying `ZIO` effect.
     */
    def flatMap[I1, R1, E1 >: E, O1](f: O => ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, O1] =
      ZArrow(((in: I, in1: I1) => zArrow(in).flatMap(f(_)(in1))).tupled)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies this `ZArrow` to `in` and the resulting `ZIO` success
     * is mapped by the given `f` function which returns another `ZArrow` which
     * is applied to `in1`. When executing, it will catch any exception thrown
     * by `f` and translate it into a dying `ZIO` effect.
     */
    def >>=[I1, R1, E1 >: E, O1](f: O => ZArrow[I1, R1, E1, O1]): ZArrow[(I, I1), R & R1, E1, O1] =
      flatMap(f)

    /**
     * Returns a `ZArrow` that maps tuples of shape `(in: I, in1: I1)`. When
     * applied, it applies this `ZArrow` to `in` and the resulting `ZIO` failure
     * is mapped by the given `f` function which returns another `ZArrow` which
     * is applied to `in1`. When executing, it will catch any exception thrown
     * by `f` and translate it into a dying `ZIO` effect.
     */
    def flatMapError[I1, R1, E1](f: E => ZArrow[I1, R1, E1, E1]): ZArrow[(I, I1), R & R1, E1, O] =
      ZArrow(((in: I, in1: I1) => zArrow(in).flatMapError(f(_)(in1).fold(id, id))).tupled)

    /**
     * Returns a `ZArrow` that maps tuples of shape `((in: I, in1: I1), in2:
     * I2)`. When applied, it applies this `ZArrow` to `in` and the resulting
     * `ZIO` success is mapped by the given `fA` function which returns another
     * `ZArrow` which is applied to `in1`. Any resulting `ZIO` failure is also
     * mapped by the given `fB` function which returns another `ZArrow` which is
     * applied to `in2`. When executing, it will catch any exception thrown by
     * `fA` or `fB` and translate it into a dying `ZIO` effect.
     */
    def flatMapBoth[I1, I2, R1, R2, E1 >: E, E2, O1](
      fE: E1 => ZArrow[I2, R2, E2, E2],
      fO: O => ZArrow[I1, R1, E1, O1]
    ): ZArrow[((I, I1), I2), R & R1 & R2, E2, O1] =
      flatMap(fO).flatMapError(fE)

    /**
     * Returns a `ZArrow` that applies the given predicate `f` to the `ZIO`
     * successes this `ZArrow` returns. It will map any success value `v` to
     * `Some(v)` when the predicate evaluates to `true`. Otherwise, it will map
     * `v` to `None`. When executing, it will catch any exception thrown by `f`
     * and translate it into a dying `ZIO` effect.
     */
    def withFilter(f: O => Boolean): ZArrow[I, R, E, Option[O]] =
      map(o => if (f(o)) Option(o) else None)
  }

  implicit class EEqualsOZArrowOps[I, R, O](zArrow: ZArrow[I, R, O, O]) {

    /**
     * Returns a `ZArrow` that applies the given `ZArrow` and whose resulting
     * `ZIO` failures are mapped by this `ZArrow`. Both successes and failures
     * of this `ZArrow` are forwarded in the error channel of the resulting
     * `ZIO`.
     */
    def errorCompose[I1, R1, O1](another: ZArrow[I1, R1, I, O1]): ZArrow[I1, R1 & R, O, O1] =
      another.errorAndThen[R, O](zArrow)
  }

  implicit class EThrowableZArrowOps[I, R, E <: Throwable, O](zArrow: ZArrow[I, R, E, O]) {

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * successes are mapped by the given `f` function. When executing, it will
     * catch any exception thrown by `f` and translate it into a failing `ZIO`
     * effect.
     */
    def mapAttempt[O1](f: O => O1): ZArrow[I, R, Throwable, O1] =
      zArrow.andThen(ZArrow.attempt(f))

    /**
     * Returns a `ZArrow` that applies this `ZArrow` and whose resulting `ZIO`
     * failures are mapped by the given `f` function. When executing, it will
     * catch any exception thrown by `f` and translate it into a failing `ZIO`
     * effect.
     */
    def mapErrorAttempt[E1 <: Throwable](f: E => E1): ZArrow[I, R, Throwable, O] =
      zArrow.errorAndThen(ZArrow.attempt(f))

    /**
     * Returns a `ZArrow` that applies the given `f` function and then applies
     * this `ZArrow`, feeding the output of the former to the later. When
     * executing, it will catch any exception thrown by `f` and translate it
     * into a failing `ZIO` effect.
     */
    def imapAttempt[I1](f: I1 => I): ZArrow[I1, R, Throwable, O] =
      ZArrow.attempt(f).andThen(zArrow)
  }

  implicit class TupledZArrowOps[I1, I2, R, E, O1, O2](zArrow: ZArrow[(I1, I2), R, E, (O1, O2)]) {

    /**
     * Applies this `ZArrow` to the input `in`, returning the `ZIO` that it is
     * mapped to.
     */
    def apply(in: (I1, I2)): ZIO[R, E, (O1, O2)] =
      zArrow.f(in)

    /**
     * Applies this `ZArrow` to each element of the `Map[I1, I2]` and returns
     * the results in a new `Map[O1, O2]`.
     */
    def apply(map: Map[I1, I2]): ZIO[R, E, Map[O1, O2]] =
      ZIO.foreach(map)((k, v) => zArrow((k, v)))

    /**
     * Applies this `ZArrow` to each element of the `Map[I1, I2]` in parallel
     * and returns the results in a new `Map[O1, O2]`.
     */
    def par(map: Map[I1, I2]) =
      ZIO.foreachPar(map)((k, v) => zArrow((k, v)))
  }

  implicit class TupledInputZArrow[I, I1, R, E, O](zArrow: ZArrow[(I, I1), R, E, O]) {

    /**
     * Returns a `ZArrow` that reverts the order of the tuples this `ZArrow`
     * maps from.
     */
    def swapInputs: ZArrow[(I1, I), R, E, O] =
      ZArrow(((in1: I1, in: I) => zArrow((in, in1))).tupled)
  }

  implicit class TupledOutputZArrow[I, R, E, O, O1](zArrow: ZArrow[I, R, E, (O, O1)]) {

    /**
     * Returns a `ZArrow` that reverts the order of the tuples of the `ZIO`
     * successes this `ZArrow` maps to.
     */
    def swapOutputs: ZArrow[I, R, E, (O1, O)] =
      zArrow.map(_.swap)

    /**
     * Returns a `ZArrow` that keeps the first component of the tuple of the
     * `ZIO` successes this `ZArrow` maps to.
     */
    def filterFirst: ZArrow[I, R, E, O] =
      zArrow.map(_._1)

    /**
     * Returns a `ZArrow` that keeps the second component of the tuple of the
     * `ZIO` successes this `ZArrow` maps to.
     */
    def filterSecond: ZArrow[I, R, E, O1] =
      zArrow.map(_._2)
  }
}
