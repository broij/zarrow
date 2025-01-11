package be.broij.zarrow

import zio._

/**
 * A `ZArrow` describes a mapping from `I` to a computation that requires an `R`
 * and either succeeds with an `O`, fails with an `E`, or dies. In other words,
 * it describes a mapping from `I` to `ZIO[R, E, O]`.
 */
final case class ZArrow[-I, -R, +E, +O] private (f: I => ZIO[R, E, O]) extends AnyVal

object ZArrow {

  /**
   * Accesses the specified ZArrow in the ZIO environment.
   */
  def service[I: Tag, R: Tag, E: Tag, O: Tag] =
    ZIO.service[ZArrow[I, R, E, O]]

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
}
