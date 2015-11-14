package org.validoc.mileage

import org.scalatest._
import org.validoc.mileage.Distance.DistanceLikeForInt
import org.validoc.mileage.Distance.DistanceLikeForShort

object CombinatorialTests {
  val distanceDefns = List(DistanceTestDefn("Int", _.toInt, Int.MaxValue), DistanceTestDefn("Short", _.toShort, Short.MaxValue))
  val matrixDefns = List(
    MatrixTestDefn("Map[(Int, Int), D]", d => Matrix.asMatrix_Map_Tuple_Like(d)),
    MatrixTestDefn("HashMap[(Int, Int), D]", d => Matrix.asMatrix_HashMap_Tuple_Like(d)),
    MatrixTestDefn("Map[Int, Map[Int, D]]", d => Matrix.asMatrix_Int_Map_Map_Like(d)),
    MatrixTestDefn("HashMap[Int, HashMap[Int, D]]", d => Matrix.asMatrix_Int_HashMap_Tuple_Like(d)),
    MatrixTestDefn("ArrayOfLocationsAndSize[D]", d => Matrix.asMatrix_Array_Like(d)),
    MatrixTestDefn("Array[Array[D]]", d => Matrix.asMatrix_Array_Array_Like(d)),
    MatrixTestDefn("Java[D]ArrayAndLength", d => d match {
      case DistanceLikeForInt   => Matrix.MatrixJavaIntArrayAndLengthLike
      case DistanceLikeForShort => Matrix.MatrixJavaShortArrayAndLengthLike
    }))

  val mileageFactoryDefns = List(
    MileageTestDefn("MileageFactory1", (d, m) => new MileageFactory1[Any, Any]()(d.asInstanceOf[Distance[Any]], m.asInstanceOf[Matrix[Any, Any]])),
    MileageTestDefn("MileageFactory2", (d, m) => new MileageFactory2[Any, Any]()(d.asInstanceOf[Distance[Any]], m.asInstanceOf[Matrix[Any, Any]])),
    MileageTestDefn("MileageFactory3", (d, m) => new MileageFactory3[Any, Any]()(d.asInstanceOf[Distance[Any]], m.asInstanceOf[Matrix[Any, Any]])))

}

case class DistanceTestDefn[D: Distance](prefix: String, toDFn: (Int) => D, large: D) {
  val distanceLike = implicitly[Distance[D]]
}
class AllDistanceLikeTests extends FlatSpec with Matchers {

  def distance[D](defn: DistanceTestDefn[D]) {
    import defn._
    implicit def toDo(x: Int) = toDFn(x)
    def zero: D = 0
    def one: D = 1
    def two: D = 2
    def three: D = 3

    s"Distance$prefix" should "implement zero and large" in {
      distanceLike.zero shouldBe zero
      distanceLike.large shouldBe large
    }

    it should "implment addition" in {
      distanceLike.add(one, two) shouldBe three
    }
    it should "implment addition with large to always return large" in {
      distanceLike.add(large, large) shouldBe large
      distanceLike.add(one, large) shouldBe large
      distanceLike.add(large, two) shouldBe large
    }
    it should "implement less than" in {
      distanceLike.lessThan(one, two) shouldBe true
      distanceLike.lessThan(two, one) shouldBe false
      distanceLike.lessThan(one, one) shouldBe false
      distanceLike.lessThan(one, large) shouldBe true
      distanceLike.lessThan(large, large) shouldBe false
      distanceLike.lessThan(large, one) shouldBe false
    }
    it should "implement less than with large numbers" in {
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

  //  "Distance[Int]" should behave like distance[Int](DistanceTestDefn("Int", _.toInt, Int.MaxValue))
  //  "Distance[Short]" should behave like distance[Short](DistanceTestDefn("Short", _.toShort, Short.MaxValue))

  CombinatorialTests.distanceDefns.foreach { defn =>
    s"Distance[${defn.prefix}]" should behave like distance(defn)
  }

}

case class MatrixTestDefn(prefix: String, matrixLike: Distance[_] => Matrix[_, _])

class AllMatrixLikeTests extends WordSpec with Matchers {

  def matrix[D, MM](distanceDefn: DistanceTestDefn[D], matrixDefn: MatrixTestDefn) {
    import distanceDefn._
    val matrixLike = matrixDefn.matrixLike(distanceLike).asInstanceOf[Matrix[D, MM]]
    import matrixLike._
    implicit def toDo(x: Int) = toDFn(x)

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
  import CombinatorialTests._
  for { d <- distanceDefns; m <- matrixDefns }
    s"Matrix[${d.prefix},${m.prefix}]" should { behave like matrix(d, m) }

}

case class MileageTestDefn(prefix: String, factoryFn: (Distance[_], Matrix[_, _]) => MileageFactory[_, _])

class AllMileageTests extends WordSpec with Matchers {

  def mileageFactory[D, MM](distanceDefn: DistanceTestDefn[D], matrixDefn: MatrixTestDefn, mileageTestDefn: MileageTestDefn) {
    import distanceDefn.distanceLike
    import distanceDefn.distanceLike._
    implicit def toDo(x: Int) = distanceDefn.toDFn(x)

    val matrixLike = matrixDefn.matrixLike(distanceLike).asInstanceOf[Matrix[D, MM]]
    implicit def toMileage(t: (Int, Int, Int)): MileageEdge[D] = MileageEdge[D](t._1, t._2, t._3)(distanceLike)

    import matrixLike._

    val mf = mileageTestDefn.factoryFn(distanceLike, matrixLike).asInstanceOf[MileageFactory[D, MM]]

    s"${mileageTestDefn.prefix}[${distanceDefn.prefix}, ${matrixDefn.prefix}]" when {
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
  import CombinatorialTests._
  for { d <- distanceDefns; m <- matrixDefns; mf <- mileageFactoryDefns }
    s"${mf.prefix}[${d.prefix},${m.prefix}]" should { behave like mileageFactory(d, m, mf) }
}
