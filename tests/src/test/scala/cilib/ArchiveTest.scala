package cilib

import scalaz._
import Scalaz._
import eu.timepit.refined.numeric.Positive
import org.scalacheck.{Gen, _}
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import eu.timepit.refined._

object GenArchive {
  def unboundedArchive: Gen[Archive[Double]] = for {
    u <- Archive.unbounded[Double]
  } yield u

  def boundedArchive: Gen[Archive[Double]] = for {
    x <- Gen.posNum[Int]
    b <- refineV[Positive](x) match {
      case Left(_) => Gen.fail
      case Right(value) => Gen.const(value)
    }
  } yield Archive.bounded(b)

  def unboundedNonEmptyArchive: Gen[Archive[Double]] = for {
    n <- Gen.chooseNum(1, 100)
    list <- Gen.listOfN(n, arbitrary[Double])
  } yield list match {
    case Nil => sys.error("this is impossible, we generate a number between 1 and 100")
    case x :: xs => Archive.unboundedNonEmpty(NonEmptyList.nel(x, x::xs.toIList))
  }

  def boundedNonEmptyArchive: Gen[Archive[Double]] = for {
    n <- Gen.chooseNum(1, 100)
    list <- Gen.listOfN(n, arbitrary[Double])
    x <- Gen.posNum[Int]
    b <- refineV[Positive](x) match {
      case Left(_) => Gen.fail
      case Right(value) => Gen.const(value)
    }
  } yield list match {
    case Nil => sys.error("this is impossible, we generate a number between 1 and 100")
    case x :: xs => {
      Archive.boundedNonEmpty(NonEmptyList.nel(x, (x::xs).toIList), b)
    }
  }
}

object ArchiveTest extends Properties("Archive") {
  property("Archive Insert") =
    forAll { archive: Archive[Double] =>
      archive.bound match {
        case Unbounded() => {
          val x = archive.insert(7.7)
          (x.size == archive.size + 1)
        }
        case Bounded(limit) => {
          if(archive.size < limit.toString().toInt){
            val x = archive.insert(7.7)
            (x.size == archive.size + 1)
          }
          else{
            val x = archive.insert(7.7)
            (x.size == archive.size)
          }
        }
      }
    }

  property("Archive toList Values") =
    forAll { archive: Archive[Double] =>
      archive.size match {
        case 0 => archive.toList == None
        case _ => archive.toList.size == archive.size
      }
    }

  property("Archive Insert with Condition (Insert Expected)") =
    forAll { archive: Archive[Double] =>
      val toBeInserted = 69.0
      val cond= (_: Double, _: Double) => true
      archive.bound match {
        case Unbounded() => {
          val result = archive.insertWith(cond)(toBeInserted)
          (result.size == archive.size + 1)
        }
        case Bounded(limit) => {
          if(archive.size < limit.value){
            val result = archive.insertWith(cond)(toBeInserted)
            (result.size == archive.size + 1)
          }
          else{
            val result = archive.insertWith(cond)(toBeInserted)
            (result.size == archive.size) && (result.size == limit) // Check... Archive management.. like most crowed deleted if full archive.
          }
        }
      }
    }

  property("Archive Insert with Condition (Insert NOT Expected)") =
  forAll { archive: Archive[Double] =>
    archive.size match {
      case 0 => {
        val notToBeInserted = 99.0
        val cond = (_: Double, _: Double) => false
        val result = archive.insertWith(cond)(notToBeInserted)
        (result.size == archive.size + 1)
      }
      case _ => {
        val notToBeInserted = 99.0
        val cond = (_: Double,_: Double) => false
        val result = archive.insertWith(cond)(notToBeInserted)
        (result.size == archive.size)
      }
    }
  }

  implicit def arbArchive: Arbitrary[Archive[Double]] =
    Arbitrary{
      Gen.frequency((1, GenArchive.unboundedArchive), (2, GenArchive.boundedArchive), (4, GenArchive.boundedNonEmptyArchive), (4, GenArchive.unboundedNonEmptyArchive))
    }
}
