package cilib.research.simulation

import java.io.File

import cilib.exec.Runner.measureWithInfo
import cilib.exec.{Measurement, Progress, Runner}
import cilib.io.csvSinkAppend
import cilib.research.core.{Archive, Benchmark}
import cilib.research.mgpso.MGParticle._
import cilib.research.mgpso._
import cilib.research.{MGArchive, _}
import cilib.{Iteration, _}
import eu.timepit.refined.auto._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO._
import scalaz.effect._
import scalaz.stream.{Process, merge}

object Simulation {

  def runIO(lambdaStrategy: LambdaStrategy,
            benchmark: Benchmark,
            iterations: Int,
            independentRuns: Int) =
    for {
      _ <- (1 to independentRuns).toList.traverse(runCount => {

        val rng = RNG.init(10L + runCount.toLong)
        val swarm = createCollection(benchmark, lambdaStrategy.evalValue(rng))

        val simulation: Process[Task, Progress[(MGArchive, NonEmptyList[MGParticle])]] = {
          Runner.foldStepS(
            placeholderENV,
            Archive.bounded[MGParticle](50, Dominates(benchmark), CrowdingDistance.mostCrowded),
            rng,
            swarm,
            Runner.staticAlgorithm(lambdaStrategy.name, Iteration.syncS(MGPSO.mgpso(benchmark))),
            benchmark.toStaticProblem,
            (x: NonEmptyList[MGParticle], _: Eval[NonEmptyList, Double]) => RVar.pure(x)
          )
        }

        val measured: Process[Task, Process[Task, Measurement[String]]] =
          Process.emitAll(List(simulation).map(_.take(iterations).pipe(measurement(runCount))))

        val benchName = benchmark.name match {
          case "ZDT1" => "zdt1"
          case "ZDT2" => "zdt2"
          case "ZDT3" => "zdt3"
          case "ZDT4" => "zdt4"
          case "ZDT6" => "zdt6"
          case "WFG1.2D" => "wfg1"
          case "WFG2.2D" => "wfg2"
          case "WFG3.2D" => "wfg3"
          case "WFG4.2D" => "wfg4"
          case "WFG5.2D" => "wfg5"
          case "WFG6.2D" => "wfg6"
          case "WFG7.2D" => "wfg7"
          case "WFG8.2D" => "wfg8"
          case "WFG9.2D" => "wfg9"
          case "WFG1.3D" => "m3_wfg1"
          case "WFG2.3D" => "m3_wfg2"
          case "WFG3.3D" => "m3_wfg3"
          case "WFG4.3D" => "m3_wfg4"
          case "WFG5.3D" => "m3_wfg5"
          case "WFG6.3D" => "m3_wfg6"
          case "WFG7.3D" => "m3_wfg7"
          case "WFG8.3D" => "m3_wfg8"
          case "WFG9.3D" => "m3_wfg9"
          case x => x
        }

        val filename = "mgpso_std_50p" + "_" + benchName + "-i500-s" + runCount.toString + ".csv"

        val stream = merge
          .mergeN(20)(measured)
          .to(csvSinkAppend[String](new File(filename)))
          .run


        clearFile(filename)
        //        clearFile(lambdaStrategy.name + "." + benchmark.name + ".500i." + runCount.toString)

        for {
          _ <- putStr(List(lambdaStrategy.name, benchmark.name, runCount).mkString(" - "))
          timeTaken <- IO {
            val start = System.nanoTime()
            stream.unsafePerformSync
            val finish = System.nanoTime()
            ((finish - start) / 1000000000).toDouble
          }
          _ <- putStr(" -- Time taken: " + timeTaken + "s")
          _ <- putStrLn("")
        } yield ()
      })
    } yield ()

  private def measurement(run: Int) =
    measureWithInfo[(MGArchive, NonEmptyList[MGParticle]), Unit, String]((info, collection) =>
      ResultsToJson.finalArchiveToString(run, info.iteration, collection._1))
    //ResultsToJson.archiveWithParticles(run, info.iteration, collection._1, collection._2))

  private def clearFile(fileName: String) = {
    val fileWriter = new java.io.PrintWriter(new File(fileName))
    fileWriter.println("")
  }

}
