package cilib
package example

import scalaz.Scalaz._
import scalaz._
import scalaz.effect._

object FileOutput extends SafeApp {

  val x: String = "45"

  def x(xs: List[Double]): Double =
    xs.suml

}
