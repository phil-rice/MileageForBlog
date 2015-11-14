package org.validoc.mileage

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.WordSpec
import java.util.HashMap

abstract class MatrixLikeTests[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) extends WordSpec with Matchers {
  val distanceLike = implicitly[Distance[D]]
  import matrixLike._
  implicit def toD(x: Int): D
  def zero: D = 0
  def one: D = 1
  def two: D = 2
  def three: D = 3
  "Matrix" when {
    "makeRaw called" should {
      "be full of the value" in {
        val m = makeRaw(5, two)
        for { i <- 0 to 4; j <- 0 to 4 } get(m, i, j) shouldBe two
      }
    }
    "put is called" should {
      "return the result" in {
        val m = makeRaw(5, three)
        val m2 = put(m, 0, 0, zero)
        val m3 = put(m2, 0, 1, one)
        val m4 = put(m3, 1, 0, two)
        get(m4, 0, 0) shouldBe zero
        get(m4, 0, 1) shouldBe one
        get(m4, 1, 0) shouldBe two
        for { i <- 2 to 4; j <- 2 to 4 } get(m4, i, j) shouldBe three
      }
    }
  }

}

abstract class MatrixLikeIntTests[MM](implicit matrixLike: Matrix[Int, MM]) extends MatrixLikeTests[Int, MM] {
  implicit def toD(x: Int) = x;
}
abstract class MatrixLikeShortTests[MM](implicit matrixLike: Matrix[Short, MM]) extends MatrixLikeTests[Short, MM] {
  implicit def toD(x: Int) = x.toShort;
}

class MatrixLikeArrayArrayIntTests extends MatrixLikeIntTests[Array[Array[Int]]]
class MatrixLikeMapMapIntTests extends MatrixLikeIntTests[Map[Int, Map[Int, Int]]]
class MatrixLikeHashMapHashMapIntTests extends MatrixLikeIntTests[HashMap[Int, HashMap[Int, Int]]]
class MatrixLikeMapTupleToIntTests extends MatrixLikeIntTests[Map[(Int, Int), Int]]
class MatrixLikeHashMapTupleToIntTests extends MatrixLikeIntTests[HashMap[(Int, Int), Int]]
class MatrixLikeArrayOfLocationsIntTests extends MatrixLikeIntTests[ArrayOfLocationsAndSize[Int]]
class MatrixLikeIntArrayAndLengthTests extends MatrixLikeIntTests[IntArrayAndLength]

class MatrixLikeArrayArrayShortTests extends MatrixLikeShortTests[Array[Array[Short]]]
class MatrixLikeMapMapShortTests extends MatrixLikeShortTests[Map[Int, Map[Int, Short]]]
class MatrixLikeHashMapHashMapShortTests extends MatrixLikeShortTests[HashMap[Int, HashMap[Int, Short]]]
class MatrixLikeMapTupleToShortTests extends MatrixLikeShortTests[Map[(Int, Int), Short]]
class MatrixLikeHashMapTupleToShortTests extends MatrixLikeShortTests[HashMap[(Int, Int), Short]]
class MatrixLikeArrayOfLocationsShortTests extends MatrixLikeShortTests[ArrayOfLocationsAndSize[Short]]
class MatrixLikeShortArrayAndLengthTests extends MatrixLikeShortTests[ShortArrayAndLength]
														

