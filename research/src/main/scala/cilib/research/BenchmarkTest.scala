package cilib.research

import cilib.research.benchmarks.zdt.ZDT
import org.uma.jmetal.problem.multiobjective
import org.uma.jmetal.solution.impl.ArrayDoubleSolution
import scalaz.Scalaz._

object BenchmarkTest extends App {

  val jmetalBenchmarks = List(
    new multiobjective.zdt.ZDT1(),
    new multiobjective.zdt.ZDT2(),
    new multiobjective.zdt.ZDT3(),
    new multiobjective.zdt.ZDT4(),
    new multiobjective.zdt.ZDT6(),
//    new multiobjective.wfg.WFG1(4, 24, 3),
//    new multiobjective.wfg.WFG2(4, 24, 3),
//    new multiobjective.wfg.WFG3(4, 24, 3),
//    new multiobjective.wfg.WFG4(4, 24, 3),
//    new multiobjective.wfg.WFG5(4, 24, 3),
//    new multiobjective.wfg.WFG6(4, 24, 3),
//    new multiobjective.wfg.WFG7(4, 24, 3),
//    new multiobjective.wfg.WFG8(4, 24, 3),
//    new multiobjective.wfg.WFG9(4, 24, 3),
  )

  val myBenchmarks = List(
    ZDT.ZDT1F _,
    ZDT.ZDT2F _,
    ZDT.ZDT3F _,
    ZDT.ZDT4F _,
    ZDT.ZDT6F _,
//    WFG.WFG1(3) _,
//    WFG.WFG2(3) _,
//    WFG.WFG3(3) _,
//    WFG.WFG4(3) _,
//    WFG.WFG5(3) _,
//    WFG.WFG6(3) _,
//    WFG.WFG7(3) _,
//    WFG.WFG8(3) _,
//    WFG.WFG9(3) _,
  )

  println({
    myBenchmarks
      .zip(jmetalBenchmarks)
      .map {
        case (f, jmetal) => {
          val solution = new ArrayDoubleSolution(jmetal)
          jmetal.evaluate(solution)

          val variables = (for (x <- 0 until solution.getNumberOfVariables)
            yield solution.getVariableValue(x)).toList.toNel.get.map(_.toDouble)
          val jmetalStr =
            s"jmetal: ${solution.getObjectives.toList.map(x => BigDecimal(x).setScale(10, BigDecimal.RoundingMode.HALF_UP).toDouble).toString}"
          val meStr =
            s"me: ${f(variables).toList.map(x => BigDecimal(x).setScale(10, BigDecimal.RoundingMode.HALF_UP).toDouble).toString}"
          s"$jmetalStr $meStr var: ${variables}"
        }
      }
      .mkString("\n")
  })

}
