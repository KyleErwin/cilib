package cilib
package example

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.numeric.Positive
import scalaz.NonEmptyList
import scalaz.Scalaz._
import scalaz.effect.IO.putStrLn
import scalaz.effect._

object SetPSO extends SafeApp {

  val x = NonEmptyList(4).toSet


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

  // The addition of two velocities, V1⊕V2
  // ⊕ : P({+,−} * U)^2 -> P({+,−} * U)
  def add[A](v1: Set[A], v2: Set[A]): Set[A] =
    (v1 ++ v2)

  // The difference between two positions, X1⊖X2
  // ⊖ : P(U)^2 -> P({+,−} * U)
  // X1⊖X2 = {+} * (X1\X2) ∪ {−} * (X2\X1)
  def subtract(p1: Set[Int], p2: Set[Int]): Set[Pair] = {
    val xs = p1.foldLeft(Set[Pair]()) {
      case (a, c) => if (!p2.contains(c)) a + Pair(Plus(), c) else a
    }
    val ugh = p2.foldLeft(Set[Pair]()) {
      case (a, c) => if (!p1.contains(c)) a + Pair(Minus(), c) else a
    }
    xs ++ ugh
  }

  //⊗
  def multiply[A](velocity: Set[A]): RVar[Set[A]] =
    Dist.stdUniform
      .flatMap { n =>
        refineV[Positive]((n * velocity.size).toInt) match {
          case Right(refined) =>
            RVar.choices(refined, velocity).map { result =>
              result.map(_.toSet).getOrElse(velocity)
            }
          case Left(_) => RVar.pure(velocity)
        }
      }

  // The multiplication of a velocity by a scalar, n⊗V
  // ⊗ : [0,1] * P({+,−} * U) -> P({+,−} * U)
  def multiply[A](n: Double, set: Set[A]): RVar[Set[A]] =
    refineV[Positive]((n * set.size).toInt) match {
      case Right(refined) =>
        RVar.choices(refined, set).map { result =>
          result.map(_.toSet).getOrElse(set)
        }
      case Left(_) => RVar.pure(set)
    }

  // The addition of a velocity and a position, X⊞V
  // is a mapping ⊞ : P(U) * P({+,−} * U) -> P(U)
  def add(velocity: List[Pair], position: List[Int]): List[Int] =
    (position ::: velocity).flatMap {
      case Pair(Plus(), value) => List(value)
      case Pair(Minus(), value) => Nil
    }.distinct

  def indicator(r: Double, floor: Int, beta: Double Refined Positive): Int =
    if (r < (beta.value - floor.toDouble)) 1 else 0

  // N(b,S) = min{|S|, ⌊b⌋ + II{r < b − ⌊b⌋}}
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

  // The removal of elements in X(t) ∩ Y(t) ∩ bY(t) from a position X(t), ⊙−
  // Denoted b ⊙− S,
  // b⊙−S = {−} * (N(b,S)/|S| ⊗ S)
  def removal(position: Set[Int]): RVar[Set[Pair]] =
    select(position).map { selected =>
      selected.map(x => Pair(Minus(), x))
    }

  def kTournamentSelection(
                            position: Set[Int],
                            A: Set[Int],
                            k: Int,
                            objective: Set[Int] => Double
                          ): RVar[Int] =
    choose(k, A).map { set =>
      set.map(x => (x, objective(position + x))).maxBy { case (elem, fitness) => fitness }._1
    }

  def kTournamentSelection(
                            n: Int,
                            position: Set[Int],
                            A: Set[Int],
                            k: Int,
                            objective: Set[Int] => Double
                          ): RVar[Set[Int]] =
    (1 |-> n).traverse(_ => kTournamentSelection(position, A, k, objective)).map(xs => xs.toSet)

  // The addition of elements outside of X(t) ∪ Y(t) ∪ bY(t) to X(t), ⊙+
  // b⊙+k A = {+} * k-TournamentSelection(A,N(b,A))
  def addition(position: Set[Int], A: Set[Int], objective: Set[Int] => Double): RVar[Set[Int]] =
    Dist.stdUniform.map(beta => N(beta, A)) >>= {
      case Left(_) => RVar.pure(position)
      case Right(value) =>
        for {
          n <- value
          winners <- kTournamentSelection(n, position, A, 5, objective)
        } yield position ++ winners
    }

  val c1, r1, c2, r2, c3, r3, c4, r4 = 1.0
  val xt, pb, gb, A = Set[Int]()
  val objective: Set[Int] => Double = set => 10.0

  def updateVelocity() = {
    val guide1 = multiply(c1 * r1, subtract(pb, xt))
    val guide2 = multiply(c2 * r2, subtract(gb, xt))
    val guide3 = addition(xt, A, objective).map(result => multiply(c3 * r3, result))
    val guide4 = removal(xt).map(result => multiply(c4 * r4, result))
  }

  val x = Set(1)

  val pos1 = Set(1, 2, 3)
  val pos2 = Set(1, 4, 5)

  override val runc: IO[Unit] =
    for {
      _ <- putStrLn(subtract(pos1, pos2).toString())
      _ <- putStrLn("Complete.")
    } yield ()

}
