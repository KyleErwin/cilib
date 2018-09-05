package cilib
package example

import eu.timepit.refined.auto._
import scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn
import spire.implicits._
import spire.math.Interval
import cilib.pso._
import cilib.pso.Defaults._
import cilib.exec._
import cilib.io.PrettyPrinter
import PrettyPrinter._

object LBestPSO extends SafeApp {
  val bounds = Interval(-5.12, 5.12) ^ 30
  val env =
    Environment(cmp = Comparison.quality(Min),
                eval =
                  Eval.unconstrained(cilib.benchmarks.Benchmarks.spherical[NonEmptyList, Double]))

  // LBest is a network topology where every Paricle 'x' has (n/2) neighbours
  // on each side. For example, a neighbourhood size of 3 means that there is
  // a single neighbour on both sides of the current particle.

  // Define a LBest PSO and run it for a single iteration
  val cognitive: Guide[Mem[Double], Double] = Guide.pbest
  val social: Guide[Mem[Double], Double] = Guide.lbest[Mem[Double]](3)

  val lbestPSO =
    gbest(0.729844, 1.496180, 1.496180, cognitive, social)

  val swarm =
    Position.createCollection(PSO.createParticle(x => Entity(Mem(x, x.zeroed), x)))(bounds, 20)
  val iter = Iteration.sync(lbestPSO)

    val problemStream = Runner.staticProblem("spherical", env.eval)


    override val runc: IO[Unit] = {

      val process = Runner.foldStep(
          env,
          RNG.fromTime,
          swarm,
          Runner.staticAlgorithm("lbestPSO", iter),
          problemStream,
          (x: NonEmptyList[Particle[Mem[Double], Double]], _: Eval[NonEmptyList, Double]) =>
              RVar.pure(x)
      )

      val result = process.take(1000).runLast.unsafePerformSync match {
          case Some(x) => PrettyPrinter(x).render(1000)
          case None => "Error"
      }

      putStrLn(result)
  }

}
