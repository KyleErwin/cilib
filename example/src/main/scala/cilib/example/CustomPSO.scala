package cilib
package example

import cilib.pso._
import cilib.exec._
import cilib.pso.PSO.{evalParticle, stdPosition, stdVelocity, updatePBest, updateVelocity}
import eu.timepit.refined.auto._
import scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn
import spire.implicits._
import spire.math.Interval

object CustomPSO extends SafeApp {
  val bounds = Interval(-5.12, 5.12) ^ 30
  val env =
    Environment(cmp = Comparison.dominance(Min),
                eval = Eval.unconstrained((xs: NonEmptyList[Double]) =>
                  Feasible(cilib.benchmarks.Benchmarks.spherical(xs))))

  def subPSO = {
    val collection =
      Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 20)
    val x = Iteration.sync(reeeee(0.729844, 1.496180, 1.496180))
    Runner.repeat(10, x, collection)
  }


  def reeeee[S](
                w: Double,
                c1: Double,
                c2: Double
              ): NonEmptyList[Particle[Mem[Double], Double]] => Particle[Mem[Double], Double] => Step[
    Double,
    Particle[Mem[Double], Double]] =
    collection =>
      x =>
        for {
          cog <- cognitive(collection, x)
          soc <- social(collection, x)
          v <- stdVelocity(x, soc, cog, w, c1, c2)
          p <- stdPosition(x, v)
          p2 <- evalParticle(p)
          p3 <- updateVelocity(p2, v)
          updated <- updatePBest(p3)
        } yield updated

  def gbest[S](
      w: Double,
      c1: Double,
      c2: Double
  ): NonEmptyList[Particle[Mem[Double], Double]] => Particle[Mem[Double], Double] => Step[
    Double,
    Particle[Mem[Double], Double]] =
    collection =>
      x =>
        for {
          cog <- cognitive(collection, x)
          subSwarm <- subPSO
          soc <- social(subSwarm, x)
          v <- stdVelocity(x, soc, cog, w, c1, c2)
          p <- stdPosition(x, v)
          p2 <- evalParticle(p)
          p3 <- updateVelocity(p2, v)
          updated <- updatePBest(p3)
        } yield updated

  // Define a normal GBest PSO and run it for a single iteration
  val cognitive = Guide.pbest[Mem[Double], Double]
  val social = Guide.gbest[Mem[Double]]
  val gbestPSO = gbest(0.729844, 1.496180, 1.496180)

  // RVar
  val swarm =
    Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 20)
  val iter = Iteration.sync(gbestPSO)

  val problemStream = Runner.staticProblem("spherical", env.eval)

  // Our IO[Unit] that runs the algorithm, at the end of the world
  override val runc: IO[Unit] = {
    val t = Runner.foldStep(
      env,
      RNG.fromTime,
      swarm,
      Runner.staticAlgorithm("gbestPSO", iter),
      problemStream,
      (x: NonEmptyList[Particle[Mem[Double], Double]], _: Eval[NonEmptyList, Double]) =>
        RVar.pure(x)
    )

    putStrLn(t.take(10).runLast.unsafePerformSync.toString)
  }
}
