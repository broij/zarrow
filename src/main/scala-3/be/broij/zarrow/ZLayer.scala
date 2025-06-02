package be.broij.zarrow.zlayer

import be.broij.zarrow.ZArrow
import zio._

final case class Ops[I, R, E, O](zArrow: ZArrow[I, R, E, O]):
  /**
   * Constructs a ZLayer from this `ZArrow`.
   */
  def layer(using tag: Tag[ZArrow[I, R, E, O]]): ULayer[ZArrow[I, R, E, O]] =
    ZLayer.succeed(zArrow)

object Ops:
  /**
   * Accesses the specified ZArrow in the ZIO environment.
   */
  def service[I: Tag, R: Tag, E: Tag, O: Tag] =
    ZIO.service[ZArrow[I, R, E, O]]
