package cilib
package example

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric.Positive
import scalaz.Scalaz._
import scalaz.effect.IO.putStrLn
import scalaz.effect._
import spire.implicits._
import spire.math.Interval

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

  def indicator(r: Double, floor: Int, beta: Double Refined Positive): Int =
    if (r < (beta.value - floor.toDouble)) 1 else 0

  def N[A](beta: Double Refined Positive, set: Set[A]): RVar[Int] =
    Dist.stdUniform.map { r =>
      val floor = beta.value.toInt
      math.min(set.size, floor + indicator(r, floor, beta))
    }

  def N[A](beta: Double, set: Set[A]): Either[String, RVar[Int]] =
    refineV[Positive](beta).map(N(_, set))

  def choose[A](n: Int Refined Positive, set: Set[A]): RVar[Set[A]] =
    RVar.choices(n, set).map {
      case Some(list) => list.toSet
      case None => Set.empty
    }

  def choose[A](n: Int, set: Set[A]): RVar[Set[A]] =
    refineV[Positive](n) match {
      case Left(_) => RVar.pure(Set.empty)
      case Right(refined) => choose(refined, set)
    }

  def select[A](beta: Double, set: Set[A]): RVar[Set[A]] =
    N(beta, set)
      .map(_ >>= (amount => choose(amount, set)))
      .getOrElse(RVar.pure(Set.empty))

  def select[A](set: Set[A]): RVar[Set[A]] =
    Dist.stdUniform >>= (beta => select(beta, set))

  def removal(position: Set[Int]): RVar[Set[Pair]] =
    select(position).map { selected =>
      selected.map(x => Pair(Minus(), x))
    }

  def pick[A](x: Int, set: Set[A]): Option[A] =
    set.zipWithIndex.foldLeft(Option.empty[A]) {
      case (a, (elem, index)) => if (a.isEmpty && index === x) Some(elem) else a
    }

  def kTournamentSelection(
                            position: Set[Int],
                            A: Set[Int],
                            k: Int,
                            objective: Set[Int] => Double
                          ) =
    Dist.uniformInt(Interval.closed(0, A.size)).replicateM(k).map { xs =>
      xs.map(x => pick(x, A))
        .foldLeft(Option.empty[(Int, Double)]) {
          case (None, None) => None
          case (None, Some(x)) => Some((x, objective(position + x)))
          case (Some((m, f)), None) => Some((m, f))
          case (Some((m, f)), Some(x)) =>
            val xf = objective(position + x)
            if (xf < f) Some((x, xf))
            else Some((m, f))
        }
        .map { case (x, _) => x }
    }

  def kTournamentSelection(
                            position: Set[Int],
                            A: Set[Int],
                            n: Int,
                            k: Int,
                            objective: Set[Int] => Double
                          ) =
    kTournamentSelection(position, A, k, objective)
      .replicateM(n)
      .map(_ >>= {
        case Some(x) => List(x)
        case None => Nil
      })
      .map(_.toSet)

  val x = Set(1)

  val pos1 = List(1, 2, 3)
  val pos2 = List(1, 4, 5)

  override val runc: IO[Unit] =
    for {
      _ <- putStrLn(difference(pos1, pos2).toString())
      _ <- putStrLn("Complete.")
    } yield ()

}
