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

import be.broij.ZArrow._
import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.Gen

object ZArrowSpec extends ZIOSpecDefault {
  def genNonEmptyChunkOf[R, A](genA: Gen[R, A]) =
    Gen.listOf1(genA).map(NonEmptyChunk.fromCons)

  def genAny: Gen[Any, Any] =
    Gen.oneOf(Gen.int, Gen.string, Gen.boolean)

  def genTuple[R1, R2, A1, A2](genFirst: Gen[R1, A1], genSecond: Gen[R2, A2]) =
    genFirst.flatMap(first => genSecond.map((first, _)))

  def genTuple[R, A](gen: Gen[R, A]): Gen[R, (A, A)] =
    genTuple(gen, gen)

  val zio = ZIO.succeed(3)

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("ZArrow")(
      suite(".unit")(
        test("maps any input to Unit") {
          check(genAny) { any =>
            assertZIO(ZArrow.unit(any))(isUnit)
          }
        }
      ),
      suite(".identity")(
        test("maps the input to itself") {
          check(genAny) { any =>
            assertZIO(ZArrow.identity(any))(equalTo(any))
          }
        }
      ),
      suite(".succeed")(
        test("succeeds with the ouput of the provided f: I => R => O") {
          val zArrow = ZArrow.succeed { str: String => prefix: String => prefix + str }
          check(genTuple(Gen.string)) { case (str, prefix) =>
            val eff = zArrow(str).provide(ZLayer.succeed(prefix))
            assertZIO(eff)(equalTo(prefix + str))
          }
        },
        test("succeeds with the ouput of the provided f: I => O") {
          val prefix = "Hello "
          val zArrow = ZArrow.succeed { str: String => prefix + str }
          check(Gen.string) { str =>
            assertZIO(zArrow(str))(equalTo(prefix + str))
          }
        },
        test("succeeds with the provided by name parameter") {
          val expected = "Something"
          val zArrow   = ZArrow.succeed(expected)
          check(genAny) { any =>
            assertZIO(zArrow(any))(equalTo(expected))
          }
        },
        test("dies when the provided f: I => R => O throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.succeed { _: Int => _: String =>
            throw exception
            1
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).provide(ZLayer.succeed("Test")).exit)(dies(equalTo(exception)))
          }
        },
        test("dies when the provided f: I => O throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.succeed { _: Int =>
            throw exception
            1
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(exception)))
          }
        },
        test("dies when the provided by name parameter throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.succeed {
            throw exception
            1
          }
          check(genAny) { any =>
            assertZIO(zArrow(any).exit)(dies(equalTo(exception)))
          }
        }
      ),
      suite(".fromZIO")(
        test("succeeds with the ouput of the provided f: I => R1 => ZIO[R, E, O]") {
          val zArrow = ZArrow.fromZIO { str: String => prefix: String => ZIO.succeed(prefix + str) }
          check(genTuple(Gen.string)) { case (str, prefix) =>
            val eff = zArrow(str).provide(ZLayer.succeed(prefix))
            assertZIO(eff)(equalTo(prefix + str))
          }
        },
        test("succeeds with the ouput of the provided f: I => ZIO[R, E, O]") {
          val prefix = "Hello "
          val zArrow = ZArrow.fromZIO { str: String => ZIO.succeed(prefix + str) }
          check(Gen.string) { str =>
            val eff = zArrow(str)
            assertZIO(eff)(equalTo(prefix + str))
          }
        },
        test("succeeds with the provided by name parameter") {
          val expected = "Something"
          val zArrow   = ZArrow.fromZIO(ZIO.succeed(expected))
          check(genAny) { any =>
            val eff = zArrow(any)
            assertZIO(eff)(equalTo(expected))
          }
        },
        test("dies when the provided f: I => R1 => ZIO[R, E, O] throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIO { _: Int => _: String =>
            throw exception
            ZIO.succeed(1)
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).provide(ZLayer.succeed("Test")).exit)(dies(equalTo(exception)))
          }
        },
        test("dies when the provided f: I => ZIO[R, E, O] throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIO { _: Int =>
            throw exception
            ZIO.succeed(1)
          }
          check(Gen.int) { int =>
            val a = zArrow(int).exit
            assertZIO(a)(dies(equalTo(exception)))
          }
        },
        test("dies when the provided by name parameter throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIO {
            throw exception
            ZIO.succeed(1)
          }
          check(genAny) { any =>
            assertZIO(zArrow(any).exit)(dies(equalTo(exception)))
          }
        }
      ),
      suite(".attempt")(
        test("succeeds with the ouput of the provided f: I => R => O") {
          val zArrow = ZArrow.succeed { num: Int => div: Int => num / div }
          check(genTuple(Gen.int, Gen.int.filter(_ != 0))) { case (num, div) =>
            val eff = zArrow(num).provide(ZLayer.succeed(div))
            assertZIO(eff)(equalTo(num / div))
          }
        },
        test("succeeds with the ouput of the provided f: I => O") {
          val div    = 2
          val zArrow = ZArrow.succeed { num: Int => num / div }
          check(Gen.int) { num =>
            val eff = zArrow(num)
            assertZIO(eff)(equalTo(num / div))
          }
        },
        test("succeeds with the provided by name parameter") {
          val expected = 1
          val zArrow   = ZArrow.succeed(expected)
          check(genAny) { any =>
            val eff = zArrow(any)
            assertZIO(eff)(equalTo(expected))
          }
        },
        test("fails when the provided f: I => R => O throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.attempt { _: Int => _: String =>
            throw exception
            1
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).provide(ZLayer.succeed("Test")).exit)(fails(equalTo(exception)))
          }
        },
        test("fails when the provided f: I => O throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.attempt { _: Int =>
            throw exception
            1
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(exception)))
          }
        },
        test("fails when the provided by name parameter throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.attempt {
            throw exception
            1
          }
          check(genAny) { any =>
            assertZIO(zArrow(any).exit)(fails(equalTo(exception)))
          }
        }
      ),
      suite(".fromZIOAttempt")(
        test("succeeds with the ouput of the provided f: I => R1 => ZIO[R, E, O]") {
          val zArrow = ZArrow.fromZIOAttempt { str: String => prefix: String => ZIO.succeed(prefix + str) }
          check(genTuple(Gen.string)) { case (str, prefix) =>
            val eff = zArrow(str).provide(ZLayer.succeed(prefix))
            assertZIO(eff)(equalTo(prefix + str))
          }
        },
        test("succeeds with the ouput of the provided f: I => ZIO[R, E, O]") {
          val prefix = "Hello "
          val zArrow = ZArrow.fromZIOAttempt { str: String => ZIO.succeed(prefix + str) }
          check(Gen.string) { str =>
            val eff = zArrow(str)
            assertZIO(eff)(equalTo(prefix + str))
          }
        },
        test("succeeds with the provided by name parameter") {
          val expected = "Something"
          val zArrow   = ZArrow.fromZIOAttempt(ZIO.succeed(expected))
          check(genAny) { any =>
            val eff = zArrow(any)
            assertZIO(eff)(equalTo(expected))
          }
        },
        test("fails when the provided f: I => R1 => ZIO[R, E, O] throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIOAttempt { _: Int => _: String =>
            throw exception
            ZIO.succeed(1)
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).provide(ZLayer.succeed("Test")).exit)(fails(equalTo(exception)))
          }
        },
        test("fails when the provided f: I => ZIO[R, E, O] throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIOAttempt { _: Int =>
            throw exception
            ZIO.succeed(1)
          }
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(exception)))
          }
        },
        test("fails when the provided by name parameter throws") {
          val exception = new Exception("unexpected!")
          val zArrow = ZArrow.fromZIOAttempt {
            throw exception
            ZIO.succeed(1)
          }
          check(genAny) { any =>
            assertZIO(zArrow(any).exit)(fails(equalTo(exception)))
          }
        }
      ),
      suite(".apply")(
        test("maps individual inputs to the expected output") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("maps the Set to the expected Set") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.setOf(Gen.int)) { set =>
            assertZIO(zArrow(set))(equalTo(set.map(_ * 2)))
          }
        },
        test("maps the Collection to the expected Collection") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.listOf(Gen.int)) { list =>
            assertZIO(zArrow(list))(equalTo(list.map(_ * 2)))
          }
        },
        test("maps the Array to the expected Array") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.listOf(Gen.int).map(_.toArray)) { array =>
            assertZIO(zArrow(array))(equalTo(array.map(_ * 2)))
          }
        },
        test("maps the NonEmptyChunk to the expected NonEmptyChunk") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(genNonEmptyChunkOf(Gen.int)) { chunk =>
            assertZIO(zArrow(chunk))(equalTo(chunk.map(_ * 2)))
          }
        },
        test("maps the Option to the expected Option") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.option(Gen.int)) { maybe =>
            assertZIO(zArrow(maybe))(equalTo(maybe.map(_ * 2)))
          }
        },
        test("maps the Map to the expected Map") {
          val f      = (k: Int, v: Int) => (k * 2, v * 4)
          val zArrow = ZArrow.succeed(f.tupled)
          check(Gen.mapOf(Gen.int, Gen.int)) { intMap =>
            val expected = intMap.map { case (k, v) => (k * 2, v * 4) }
            assertZIO(zArrow(intMap))(equalTo(expected))
          }
        }
      ),
      suite(".par")(
        test("maps the Set to the expected Set") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.setOf(Gen.int)) { set =>
            assertZIO(zArrow.par(set))(equalTo(set.map(_ * 2)))
          }
        },
        test("maps the Collection to the expected Collection") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.listOf(Gen.int)) { list =>
            assertZIO(zArrow.par(list))(equalTo(list.map(_ * 2)))
          }
        },
        test("maps the Array to the expected Array") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(Gen.listOf(Gen.int).map(_.toArray)) { array =>
            assertZIO(zArrow.par(array))(equalTo(array.map(_ * 2)))
          }
        },
        test("maps the NonEmptyChunk to the expected NonEmptyChunk") {
          val zArrow = ZArrow.succeed { i: Int => i * 2 }
          check(genNonEmptyChunkOf(Gen.int)) { chunk =>
            assertZIO(zArrow.par(chunk))(equalTo(chunk.map(_ * 2)))
          }
        },
        test("maps the Map to the expected Map") {
          val f      = (k: Int, v: Int) => (k * 2, v * 4)
          val zArrow = ZArrow.succeed(f.tupled)
          check(Gen.mapOf(Gen.int, Gen.int)) { intMap =>
            val expected = intMap.map { case (k, v) => (k * 2, v * 4) }
            assertZIO(zArrow.par(intMap))(equalTo(expected))
          }
        }
      ),
      suite(".combine and <*>")(
        test("applies the correct ZArrow to each component of the tuple") {
          val adder          = ZArrow.succeed { int: Int => int + 1 }
          val multiplier     = ZArrow.succeed { int: Int => int * 2 }
          val combineZArrow1 = adder.combine(multiplier)
          val combineZArrow2 = adder <*> multiplier
          check(genTuple(Gen.int)) { case (a, b) =>
            val assertion = equalTo((a + 1, b * 2))
            assertZIO(combineZArrow1((a, b)))(assertion) &&
            assertZIO(combineZArrow2((a, b)))(assertion)
          }
        },
        test("applies the ZArrow that was combined before the one that was passed") {
          val fAdd           = (int: Int, enqueue: Enqueue[Int]) => enqueue.offer(int + 1)
          val fMultiply      = (int: Int, enqueue: Enqueue[Int]) => enqueue.offer(int * 2)
          val adder          = ZArrow.fromZIO(fAdd.tupled)
          val multiplier     = ZArrow.fromZIO(fMultiply.tupled)
          val combineZArrow1 = adder.combine(multiplier)
          val combineZArrow2 = adder <*> multiplier
          check(genTuple(Gen.int)) { case (a, b) =>
            val assertion = equalTo(Chunk(a + 1, b * 2))
            for {
              queue  <- Queue.unbounded[Int]
              _      <- combineZArrow1(((a, queue), (b, queue)))
              items1 <- queue.takeAll
              _      <- combineZArrow2(((a, queue), (b, queue)))
              items2 <- queue.takeAll
            } yield assert(items1)(assertion) && assert(items2)(assertion)
          }
        }
      ),
      suite(".combinePar and <&>")(
        test("applies the correct ZArrow to each component of the tuple") {
          val adder             = ZArrow.succeed { int: Int => int + 1 }
          val multiplier        = ZArrow.succeed { int: Int => int * 2 }
          val combineParZArrow1 = adder.combinePar(multiplier)
          val combineParZArrow2 = adder <&> multiplier
          check(genTuple(Gen.int)) { case (a, b) =>
            val assertion = equalTo(((a + 1), (b * 2)))
            assertZIO(combineParZArrow1((a, b)))(assertion) &&
            assertZIO(combineParZArrow2((a, b)))(assertion)
          }
        }
      ),
      suite(".zip")(
        test("applies the ZArrow that was zipped before the one that was passed") {
          val fAdd       = (int: Int, enqueue: Enqueue[Int]) => enqueue.offer(int + 1)
          val fMultiply  = (int: Int, enqueue: Enqueue[Int]) => enqueue.offer(int * 2)
          val adder      = ZArrow.fromZIO(fAdd.tupled)
          val multiplier = ZArrow.fromZIO(fMultiply.tupled)
          val zipZArrow  = adder.zip(multiplier)
          check(Gen.int) { int =>
            for {
              queue <- Queue.unbounded[Int]
              _     <- zipZArrow((int, queue))
              items <- queue.takeAll
            } yield assert(items)(equalTo(Chunk(int + 1, int * 2)))
          }
        }
      ),
      suite(".zipPar")(
        test("applies both ZArrow to the input") {
          val adder        = ZArrow.succeed { int: Int => int + 1 }
          val multiplier   = ZArrow.succeed { int: Int => int * 2 }
          val zipParZArrow = adder.zipPar(multiplier)
          check(Gen.int) { int =>
            assertZIO(zipParZArrow(int))(equalTo((int + 1, int * 2)))
          }
        }
      ),
      suite(".compose and <<<")(
        test("applies the original ZArrow to the zio successes returned by the one that was passed") {
          val adder          = ZArrow.succeed { int: Int => int + 1 }
          val multiplier     = ZArrow.succeed { int: Int => int * 2 }
          val composeZArrow1 = adder.compose(multiplier)
          val composeZArrow2 = adder <<< multiplier
          check(Gen.int) { int =>
            val assertion = equalTo((int * 2) + 1)
            assertZIO(composeZArrow1(int))(assertion) &&
            assertZIO(composeZArrow2(int))(assertion)
          }
        }
      ),
      suite(".andThen and >>>")(
        test("applies the ZArrow that was passed to the zio successes returned by the original one") {
          val adder          = ZArrow.succeed { int: Int => int + 1 }
          val multiplier     = ZArrow.succeed { int: Int => int * 2 }
          val andThenZArrow1 = adder.andThen(multiplier)
          val andThenZArrow2 = adder >>> multiplier
          check(Gen.int) { int =>
            val assertion = equalTo((int + 1) * 2)
            assertZIO(andThenZArrow1(int))(assertion) &&
            assertZIO(andThenZArrow2(int))(assertion)
          }
        }
      ),
      suite(".errorCompose")(
        test("applies the original ZArrow to the zio failures returned by the one that was passed") {
          val thrower            = ZArrow.fromZIO { int: Int => ZIO.fail(int) }
          val errorHandler       = ZArrow.succeed { int: Int => int * 2 }
          val errorComposeZArrow = errorHandler.errorCompose(thrower)
          check(Gen.int) { int =>
            assertZIO(errorComposeZArrow(int).exit)(fails(equalTo(int * 2)))
          }
        }
      ),
      suite(".errorAndThen")(
        test("applies the ZArrow that was passed to the zio failures returned by the original one") {
          val thrower            = ZArrow.fromZIO { int: Int => ZIO.fail(int) }
          val errorHandler       = ZArrow.succeed { int: Int => int * 2 }
          val errorAndThenZArrow = thrower.errorAndThen(errorHandler)
          check(Gen.int) { int =>
            assertZIO(errorAndThenZArrow(int).exit)(fails(equalTo(int * 2)))
          }
        }
      ),
      suite("catchAll")(
        test(
          "applies the provided ZArrow to the failures returned by the original ZArrow and recovers in case it returns a succeeding ZIO"
        ) {
          val failingZArrow = ZArrow.fromZIO { i: Int => ZIO.fail(i) }
          val zArrow        = failingZArrow.catchAll(ZArrow.identity)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int))
          }
        },
        test(
          "applies the provided ZArrow to the failures returned by the original ZArrow and doesn't recover in case it returns a failing ZIO"
        ) {
          val failingZArrow = ZArrow.fromZIO { i: Int => ZIO.fail(i) }
          val zArrow        = failingZArrow.catchAll(failingZArrow)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(int)))
          }
        }
      ),
      suite("catchAllCause")(
        test(
          "applies the provided ZArrow to the failures returned by the original ZArrow and recovers in case it returns a succeeding ZIO"
        ) {
          val failingZArrow = ZArrow.fromZIO { i: Int => ZIO.fail(i) }
          val expected      = "recovered"
          val zArrow        = failingZArrow.catchAllCause(ZArrow.succeed(expected))
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(expected))
          }
        },
        test(
          "applies the provided ZArrow to the failures returned by the original ZArrow and doesn't recover in case it returns a failing ZIO"
        ) {
          val failingZArrow = ZArrow.fromZIO { i: Int => ZIO.fail(i) }
          val catcherZArrow = ZArrow.fromZIO { _: Cause[Int] => ZIO.fail(1) }
          val zArrow        = failingZArrow.catchAllCause(catcherZArrow)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(1)))
          }
        },
        test(
          "applies the provided ZArrow to the defects returned by the original ZArrow and recovers in case it returns a succeeding ZIO"
        ) {
          val exception   = new Exception("unexpected!")
          val expected    = "recovered"
          val dyingZArrow = ZArrow.fromZIO(ZIO.die(exception))
          val zArrow      = dyingZArrow.catchAllCause(ZArrow.succeed(expected))
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(expected))
          }
        },
        test(
          "applies the provided ZArrow to the defects returned by the original ZArrow and doesn't recover in case it returns a failing ZIO"
        ) {
          val exception     = new Exception("unexpected!")
          val dyingZArrow   = ZArrow.fromZIO[Any, Nothing, Int](ZIO.die(exception))
          val catcherZArrow = ZArrow.fromZIO { _: Cause[Int] => ZIO.fail(1) }
          val zArrow        = dyingZArrow.catchAllCause(catcherZArrow)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(1)))
          }
        }
      ),
      suite(".mapZIO")(
        test("applies the provided function to the zio successes returned by the original ZArrow") {
          val fMultiply = (zio: ZIO[Any, Nothing, Int]) => zio.map(_ * 2)
          val zArrow    = ZArrow.identity[Int].mapZIO(fMultiply)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("applies the provided function to the zio failures returned by the original ZArrow") {
          val failingZArrow = ZArrow.fromZIO { int: Int => ZIO.fail(int) }
          val fMultiply     = (zio: ZIO[Any, Int, Nothing]) => zio.catchAll(int => ZIO.succeed(int * 2))
          val zArrow        = failingZArrow.mapZIO(fMultiply)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("dies when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: ZIO[Any, Nothing, Int]) => throw expected
          val zArrow   = ZArrow.identity[Int].mapZIO(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite(".map")(
        test("applies the provided function to the zio successes returned by the original ZArrow") {
          val zArrow = ZArrow.identity[Int].map(_ * 2)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("dies when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.identity[Int].map(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite(".mapAttempt")(
        test("applies the provided function to the zio succcesses returned by the original ZArrow") {
          val zArrow = ZArrow.identity[Int].mapAttempt(_ * 2)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("fails when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.identity[Int].mapAttempt(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(expected)))
          }
        }
      ),
      suite(".mapError")(
        test("applies the provided function to the zio failures returned by the original ZArrow") {
          val zArrow = ZArrow.fromZIO { int: Int => ZIO.fail(int) }.mapError(_ * 2)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(int * 2)))
          }
        },
        test("dies when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.fromZIO { int: Int => ZIO.fail(int) }.mapError(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite("mapErrorAttempt")(
        test("applies the provided function to the zio failures returned by the original ZArrow") {
          val exception = new Exception("BOOM")
          val expected  = new Exception("BAM")
          val zArrow    = ZArrow.fromZIO { _: Int => ZIO.fail(exception) }.mapErrorAttempt(_ => expected)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(expected)))
          }
        },
        test("fails when the provided function throws") {
          val exception = new Exception("BOOM")
          val expected  = new Exception("BAM")
          val f         = (_: Exception) => throw expected
          val zArrow    = ZArrow.fromZIO { _: Int => ZIO.fail(exception) }.mapErrorAttempt(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(expected)))
          }
        }
      ),
      suite(".mapBoth")(
        test("applies the provided function fO to the zio succcesses returned by the original ZArrow") {
          val zArrow = ZArrow.identity[Int].mapBoth(scala.Predef.identity, _ * 2)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(equalTo(int * 2))
          }
        },
        test("dies when the provided function fO throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.identity[Int].mapBoth(scala.Predef.identity, f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        },
        test("applies the provided function fE to the zio failures returned by the original ZArrow") {
          val zArrow = ZArrow.fromZIO { int: Int => ZIO.fail(int) }.mapBoth(_ * 2, scala.Predef.identity)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(int * 2)))
          }
        },
        test("dies when the provided function fE throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.fromZIO { int: Int => ZIO.fail(int) }.mapBoth(f, scala.Predef.identity)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite(".imap")(
        test("applies the provided function to the inputs and forwards its output to the original ZArrow") {
          val zArrow = ZArrow.succeed { int: Int => int * 2 }.imapAttempt { str: String => str.toInt }
          check(Gen.int) { int =>
            assertZIO(zArrow(int.toString))(equalTo(int * 2))
          }
        },
        test("dies when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.identity[Int].imap(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite(".imapAttempt")(
        test("applies the provided function to the inputs and forwards its output to the original ZArrow") {
          val zArrow = ZArrow.succeed { int: Int => int * 2 }.imapAttempt { str: String => str.toInt }
          check(Gen.int) { int =>
            assertZIO(zArrow(int.toString))(equalTo(int * 2))
          }
        },
        test("fails when the provided function throws") {
          val expected = new Exception("unexpected")
          val f        = (_: Int) => throw expected
          val zArrow   = ZArrow.identity[Int].imapAttempt(f)
          check(Gen.int) { int =>
            assertZIO(zArrow(int).exit)(fails(equalTo(expected)))
          }
        }
      ),
      suite(".flatMap and >>=")(
        test("applies the provided function and the ZArrow it gives to the successes of the origianl ZArrow") {
          val adder   = ZArrow.succeed { int: Int => int * 2 }
          val f       = (a: Int) => ZArrow.succeed { b: Int => (b * a) + 1 }
          val zArrow1 = adder.flatMap(f)
          val zArrow2 = adder >>= f
          check(genTuple(Gen.int)) { case (a, b) =>
            val assertion = equalTo(((a * 2) * b) + 1)
            assertZIO(zArrow1((a, b)))(assertion) &&
            assertZIO(zArrow2((a, b)))(assertion)
          }
        },
        test("dies when the provided function throws") {
          val expected = new Exception("unexpected")
          val f = (_: Int) => {
            throw expected
            ZArrow.identity[Int]
          }
          val zArrow1 = ZArrow.identity[Int].flatMap(f)
          val zArrow2 = ZArrow.identity[Int] >>= f
          check(Gen.int) { int =>
            val assertion = dies(equalTo(expected))
            assertZIO(zArrow1((int, int)).exit)(assertion) &&
            assertZIO(zArrow2((int, int)).exit)(assertion)
          }
        }
      ),
      suite(".flatMapError")(
        test("applies the provided function and the ZArrow it gives to the failures of the original ZArrow") {
          val failingAdder = ZArrow.fromZIO { int: Int => ZIO.fail(int * 2) }
          val zArrow = failingAdder.flatMapError { a: Int =>
            ZArrow.succeed { b: Int => (b * a) + 1 }
          }
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow((a, b)).exit)(fails(equalTo(((a * 2) * b) + 1)))
          }
        },
        test("dies when the provided function throws") {
          val failingAdder = ZArrow.fromZIO { int: Int => ZIO.fail(int * 2) }
          val expected     = new Exception("unexpected")
          val f = (_: Int) => {
            throw expected
            ZArrow.identity[Int]
          }

          val zArrow = failingAdder.flatMapError(f)
          check(Gen.int) { int =>
            assertZIO(zArrow((int, int)).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite(".flatMapBoth")(
        test("applies fO and the ZArrow it gives to the successes of the original ZArrow") {
          val adder         = ZArrow.succeed { int: Int => int * 2 }
          val followUp      = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 1)
          val errorFollowUp = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 2)
          val zArrow        = adder.flatMapBoth(errorFollowUp, followUp)
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow(((a, b), b)))(equalTo(((a * 2) * b) + 1))
          }
        },
        test("applies fE and the ZArrow it gives to the failures of the original ZArrow") {
          val failingAdder  = ZArrow.fromZIO { int: Int => ZIO.fail(int * 2) }
          val followUp      = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 1)
          val errorFollowUp = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 2)
          val zArrow        = failingAdder.flatMapBoth(errorFollowUp, followUp)
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow(((a, b), b)).exit)(fails(equalTo(((a * 2) * b) + 2)))
          }
        },
        test("dies when fO throws") {
          val adder         = ZArrow.succeed { int: Int => int * 2 }
          val expected      = new Exception("unexpected")
          val followUp      = (_: Int) => throw expected
          val errorFollowUp = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 2)
          val zArrow        = adder.flatMapBoth(errorFollowUp, followUp)
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow(((a, b), b)).exit)(dies(equalTo(expected)))
          }
        },
        test("dies when fE throws") {
          val failingAdder  = ZArrow.fromZIO { int: Int => ZIO.fail(int * 2) }
          val followUp      = (a: Int) => ZArrow.identity[Int].map(b => (b * a) + 1)
          val expected      = new Exception("unexpected")
          val errorFollowUp = (_: Int) => throw expected
          val zArrow        = failingAdder.flatMapBoth(errorFollowUp, followUp)
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow(((a, b), b)).exit)(dies(equalTo(expected)))
          }
        }
      ),
      suite("withFilter")(
        test(
          "returns a ZArrow that succeeds with Some(s) when the predicate gives true for the successes s of the original ZArrow"
        ) {
          val zArrow = ZArrow.identity[Int].withFilter(_ => true)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(isSome(equalTo(int)))
          }
        },
        test(
          "returns a ZArrow that succeeds with None when the predicate gives false for the successes of the original ZArrow"
        ) {
          val zArrow = ZArrow.identity[Int].withFilter(_ => false)
          check(Gen.int) { int =>
            assertZIO(zArrow(int))(isNone)
          }
        }
      ),
      suite(".swapInputs")(
        test("reverts the order of the input tuples") {
          val idInt    = ZArrow.identity[Int]
          val toString = ZArrow.identity[Any].map(_.toString())
          val zArrow   = idInt.combine(toString).swapInputs
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow((a, b)))(equalTo((b, a.toString())))
          }
        }
      ),
      suite(".swapOutputs")(
        test("reverts the order of the output tuples") {
          val idInt    = ZArrow.identity[Int]
          val toString = ZArrow.identity[Any].map(_.toString())
          val zArrow   = idInt.combine(toString).swapOutputs
          check(genTuple(Gen.int)) { case (a, b) =>
            assertZIO(zArrow((a, b)))(equalTo((b.toString(), a)))
          }
        }
      )
    )
}
