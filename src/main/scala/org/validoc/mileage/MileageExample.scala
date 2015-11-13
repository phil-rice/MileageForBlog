package org.validoc.mileage

object MileageDemo {
  import scala.language.implicitConversions

  def simple {
    val mf = new MileageFactory1[Int, Array[Array[Int]]]
    implicit def toMileage(t: (Int, Int, Int)) = MileageEdge(t._1, t._2, t._3)
    val mm = mf(3, List((0, 1, 10), (0, 2, 20), (1, 2, 25)))

    println("simple")
    println("0 to 1 should be 10 " + mm(0, 1))
    println("0 to 2 should be 20 " + mm(0, 2))
    println("1 to 2 should be 25 " + mm(0, 1))
  }

  def withCalculation {
    val mf = new MileageFactory1[Int, Array[Array[Int]]]
    implicit def toMileage(t: (Int, Int, Int)) = MileageEdge(t._1, t._2, t._3)
    val mm = mf(3, List((0, 1, 10), (0, 2, 20)))

    println("with calculation")
    println("0 to 1 should be 10 " + mm(0, 1))
    println("0 to 2 should be 20 " + mm(0, 2))
    println("1 to 2 should be 30 " + mm(0, 1))
  }

  def main(args: Array[String]): Unit = {
    simple
    withCalculation
  }

}