package cilib
package research

import cilib.research.core.BenchmarkSuite
import cilib.research.mgpso.LambdaStrategy
import cilib.research.simulation.Simulation
import scalaz._
import scalaz.effect.IO._
import scalaz.effect.SafeApp

object Main extends SafeApp {

  // args -> lambda strategy, benchmark suite
  override def run(args: ImmutableArray[String]) = {

    val benchmarkSuite = args(1) match {
    case "ZDT"    => BenchmarkSuite.ZDT
    case "WFG.2D" => BenchmarkSuite.WFG_2D
    case "WFG.3D" => BenchmarkSuite.WFG_3D
  }

    val simulationsIO = benchmarkSuite.benchmarks.traverse1(benchmark => {
      val bounds = benchmark.bounds

      val lambdaStrategy = args(0) match {
        case "STD" => LambdaStrategy.Standard(bounds)
        case "LI"  => LambdaStrategy.Standard(bounds)
        case "LD"  => LambdaStrategy.Standard(bounds)
        case "R"   => LambdaStrategy.Standard(bounds)
        case "RI"  => LambdaStrategy.Standard(bounds)
        case "RIJ" => LambdaStrategy.Standard(bounds)
      }

      Simulation.runIO(lambdaStrategy, benchmark, 2000, 4)
    })

    for {
      _ <- putStrLn("Starting")
      _ <- simulationsIO
      _ <- putStrLn("Done")
    } yield ()

  }

}