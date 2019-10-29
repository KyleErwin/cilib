package cilib
package example

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz.effect.IO.putStrLn
import scalaz.effect._

object SetPSO extends SafeApp {

  sealed abstract class Operation {
    def run(x: Double, y: Double): Double =
      this match {
        case Minus() => x - y
        case Plus()  => x + y
      }
  }
  final case class Minus() extends Operation
  final case class Plus() extends Operation

  refineV[Positive](52)
  final case class Pair(operation: Operation, value: Int)

  def addition(v1: List[Pair], v2: List[Pair]): List[Pair] =
    (v1 ::: v2).distinct

  def difference(p1: List[Int], p2: List[Int]): List[Pair] = {
    val xs = p1.foldLeft(List[Pair]()) {
      case (a, c) => if (!p2.contains(c)) Pair(Plus(), c) :: a else a
    }
    val ugh = p2.foldLeft(List[Pair]()) {
      case (a, c) => if (!p1.contains(c)) Pair(Minus(), c) :: a else a
    }
    xs ::: ugh
  }

  def multiplicationByScalar[A](velocity: List[Pair]): RVar[List[Pair]] =
    Dist.stdUniform
      .flatMap { n =>
        refineV[Positive]((n * velocity.size).toInt) match {
          case Right(refined) =>
            RVar.choices(refined, velocity).map { result =>
              result.getOrElse(velocity)
            }
          case Left(_) => RVar.pure(velocity)
        }
      }

  def addition2(velocity: List[Pair], position: List[Int]): List[Int] =
    (position ::: velocity).flatMap {
      case Pair(Plus(), value) => List(value)
      case Pair(Minus(), value) => Nil
    }.distinct

  def indicator(r: Double, floor: Int, beta: Double): Int =
    if (r < (beta - floor.toDouble)) 1 else 0

  def N[A](beta: Double Refined Positive, set: Set[A]): RVar[Int] =
    Dist.stdUniform.map { r =>
      val floor = beta.value.toInt
      math.min(set.size, floor + indicator(r, floor, beta))
    }

  def choose[A](n: Int Refined Positive, set: Set[A]): RVar[Set[A]] =
    RVar.choices(n, set).map {
      case Some(list) => list.toSet
      case None => Set.empty
    }

  def choose[A](n: Int, set: Set[A]): RVar[Set[A]] =
    setFromEither[Int, A](refineV[Positive](n), x => choose(x, set))

  def setFromEither[A, B](
                           either: Either[String, Refined[A, Positive]],
                           f: Refined[A, Positive] => RVar[Set[B]]
                         ): RVar[Set[B]] =
    either match {
      case Left(_) => RVar.pure(Set.empty[B])
      case Right(refined) => f(refined)
    }

  def select[A](set: Set[A]): RVar[Set[A]] =
    Dist.stdUniform >>= { beta =>
      setFromEither(refineV[Positive](beta), value => N(value, set) >>= (x => choose(x, set)))
    }

  //  def select[A](set: Set[A]): RVar[Set[A]] =
  //    for {
  //    beta <- Dist.stdUniform
  //    n <- N()
  //    }
  //
  //
  //  def removal(position: Set[Int]): RVar[Set[Pair]] =
  //    for {
  //      beta <- Dist.stdUniform
  //      selected <- N(beta, position)
  //    } yield selected.map(Pair(Minus(), _))

  val pos1 = List(1, 2, 3)
  val pos2 = List(1, 4, 5)

  override val runc: IO[Unit] =
    for {
      _ <- putStrLn(difference(pos1, pos2).toString())
      _ <- putStrLn("Complete.")
    } yield ()

}
