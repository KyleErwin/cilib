package cilib
package example

import scalaz.Scalaz._
import scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive

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

  def multiplcationByScalar[A](velocity: List[A]): RVar[List[A]] =
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

  val pos1 = List(1, 2, 3)
  val pos2 = List(1, 4, 5)
  override val runc: IO[Unit] =
    for {
      _ <- putStrLn(difference(pos1, pos2).toString())
      _ <- putStrLn("Complete.")
    } yield ()

}
