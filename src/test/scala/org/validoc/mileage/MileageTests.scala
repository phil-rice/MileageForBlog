package org.validoc.mileage

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.WordSpec
import java.util.HashMap

abstract class MileageTests[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) extends WordSpec with Matchers {
  val mf = new MileageFactory1[D, MM]
  implicit def toMileage(t: (Int, Int, Int)): MileageEdge[D]
  implicit def toD(x: Int): D
  implicit val distanceLike = implicitly[Distance[D]]
  import distanceLike._

  "A mileage matrix" when {
    val mm = mf(3, List())
    "empty" should {
      "have a zero for same from/to" in {
        mm(0, 0) shouldBe zero
        mm(1, 1) shouldBe zero
        mm(2, 2) shouldBe zero
      }
      "have a large value for other mileages" in {
        for { i <- 0 to 2; j <- 0 to 2 if i != j } {
          mm(i, j).getClass shouldBe large.getClass
          mm(i, j) shouldBe large
          mm(j, i) shouldBe large
        }
      }
    }

    "all combinations given" should {
      "return 0 for form/to being the same" in {
        val mm = mf(3, List((0, 1, 10), (0, 2, 20), (1, 2, 25)))
        mm(0, 0) shouldBe 0
        mm(1, 1) shouldBe 0
        mm(2, 2) shouldBe 0
      }
      "return the stated combinations" in {
        val mm = mf(3, List((0, 1, 10), (0, 2, 20), (1, 2, 25)))
        mm(0, 1) shouldBe 10
        mm(0, 2) shouldBe 20
        mm(1, 2) shouldBe 25
      }
      "return the inverse combinations" in {
        val mm = mf(3, List((0, 1, 10), (0, 2, 20), (1, 2, 25)))
        mm(1, 0) shouldBe 10
        mm(2, 0) shouldBe 20
        mm(2, 1) shouldBe 25
      }
    }
    "some combinations not given" should {
      val mm = mf(3, List((0, 1, 10), (0, 2, 20)))
      "do the sum of links if longer" in {
        mm(2, 1) shouldBe 30
        mm(1, 2) shouldBe 30
      }
    }
  }

}

abstract class MileageIntTests[MM](implicit matrixLike: Matrix[Int, MM]) extends MileageTests[Int, MM] {
  implicit def toD(x: Int) = x;
  implicit def toMileage(t: (Int, Int, Int)) = MileageEdge(t._1, t._2, t._3)
}
abstract class MileageShortTests[MM](implicit matrixLike: Matrix[Short, MM]) extends MileageTests[Short, MM] {
  implicit def toD(x: Int) = x.toShort;
  implicit def toMileage(t: (Int, Int, Int)) = MileageEdge(t._1, t._2, t._3.toShort)
}

class MileageArrayArrayIntTests extends MileageIntTests[Array[Array[Int]]]
class MileageMapMapIntTests extends MileageIntTests[Map[Int, Map[Int, Int]]]
class MileageHashMapHashMapIntTests extends MileageIntTests[HashMap[Int, HashMap[Int, Int]]]
class MileageMapTupleToIntTests extends MileageIntTests[Map[(Int, Int), Int]]
class MileageHashMapTupleToIntTests extends MileageIntTests[HashMap[(Int, Int), Int]]
class MileageArrayOfLocationsIntTests extends MileageIntTests[ArrayOfLocationsAndSize[Int]]
class MileageIntArrayAndLengthTests extends MileageIntTests[IntArrayAndLength]

class MileageArrayArrayShortTests extends MileageShortTests[Array[Array[Short]]]
class MileageMapMapShortTests extends MileageShortTests[Map[Int, Map[Int, Short]]]
class MileageHashMapHashMapShortTests extends MileageShortTests[HashMap[Int, HashMap[Int, Short]]]
class MileageMapTupleToShortTests extends MileageShortTests[Map[(Int, Int), Short]]
class MileageHashMapTupleToShortTests extends MileageShortTests[HashMap[(Int, Int), Short]]
class MileageArrayOfLocationsShortTests extends MileageShortTests[ArrayOfLocationsAndSize[Short]]
class MileageShortArrayAndLengthTests extends MileageShortTests[ShortArrayAndLength]
                            

