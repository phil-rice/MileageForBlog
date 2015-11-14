package org.validoc.mileage

import org.scalatest.FlatSpec
import org.scalatest.Matchers

abstract class DistanceTests[D: Distance] extends FlatSpec with Matchers {
  val distanceLike = implicitly[Distance[D]]
  implicit def toD(x: Int): D
  def zero: D = 0
  def one: D = 1
  def two: D = 2
  def three: D = 3
  def large: D

  "Distance" should "implement zero and large" in {
    distanceLike.zero shouldBe zero
    distanceLike.large shouldBe large
  }

it  should "implment addition" in {
    distanceLike.add(one, two) shouldBe three
  }
 it  should "implment addition with large to always return large" in {
	  distanceLike.add(large, large) shouldBe large
	  distanceLike.add(one, large) shouldBe large
	  distanceLike.add(large, two) shouldBe large
  }
it  should "implement less than" in {
    distanceLike.lessThan(one, two) shouldBe true
    distanceLike.lessThan(two, one) shouldBe false
    distanceLike.lessThan(one, one) shouldBe false
    distanceLike.lessThan(one, large) shouldBe true
    distanceLike.lessThan(large, large) shouldBe false
    distanceLike.lessThan(large, one) shouldBe false
  }
it  should "implement less than with large numbers" in {
	  distanceLike.lessThan(one, large) shouldBe true
	  distanceLike.lessThan(one, distanceLike.add(large, two)) shouldBe true
  }
it should "implement makeArray" in {
    val array = distanceLike.makeArray(5, one)
    array.length shouldBe 5
    for (i <- 0 to 4) array(i) shouldBe one
  }
  
  it should "implement makeArrayArray" in {
    val array = distanceLike.makeArrayArray(5, one)
    array.length shouldBe 5
    for (i <- 0 to 4) {
      val a = array(i)
      a.length shouldBe 5
      for (j <- 0 to 4) a(j) shouldBe one
    }
  }

}

class DistanceIntTests extends DistanceTests[Int] { implicit def toD(x: Int) = x; val large = Int.MaxValue }
class DistanceShortTests extends DistanceTests[Short] { implicit def toD(x: Int) = x.toShort; val large = Short.MaxValue }

