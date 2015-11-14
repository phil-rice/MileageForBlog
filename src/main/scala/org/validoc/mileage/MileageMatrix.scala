package org.validoc.mileage

import java.util.HashMap
import scala.util.Random
import scala.reflect.ClassTag

trait Distance[D] {
  def zero: D
  def large: D
  def random: D
  def add(d1: D, d2: D): D = if (d1== large|| d2==large) large else addRaw(d1, d2) 
  def addRaw(d1: D, d2: D): D
  def lessThan(d1: D, d2: D): Boolean
  def makeArray(size: Int, value: D): Array[D] //needed because Arrays need ClassTags which are painful
  def makeArrayArray(size: Int, value: D): Array[Array[D]] //needed because Arrays need ClassTags which are painful
}

object Distance {
  implicit object DistanceLikeForInt extends Distance[Int] {
    def zero: Int = 0
    def large = Int.MaxValue
    def random = Random.nextInt(100000)
    def addRaw(d1: Int, d2: Int) = d1 + d2
    def lessThan(d1: Int, d2: Int) = d1 < d2
    def makeArray(size: Int, value: Int) = Array.fill(size)(value)
    def makeArrayArray(size: Int, value: Int) = Array.fill(size)(Array.fill(size)(value))
  }

  implicit object DistanceLikeForShort extends Distance[Short] {
    def zero: Short = 0
    def large = Short.MaxValue
    def random = Random.nextInt(Short.MaxValue).toShort
    def addRaw(d1: Short, d2: Short) = (d1 + d2).toShort
    def lessThan(d1: Short, d2: Short) = d1 < d2
    def makeArray(size: Int, initialValue: Short) = Array.fill(size)(initialValue)
    def makeArrayArray(size: Int, initialValue: Short) = Array.fill(size)(Array.fill(size)(initialValue))
  }
}

case class MileageEdge[D: Distance](from: Int, to: Int, distance: D)

trait Matrix[D, MM] {
  def makeRaw(numberOfLocations: Int, value: D): MM
  def get(hs: MM, l1: Int, L2: Int): D
  def put(hs: MM, l1: Int, l2: Int, d: D): MM

  def addIfBetter(mm: MM, k: Int, i: Int, j: Int)(implicit distanceLike: Distance[D]) = {
    val newDistance = distanceLike.add(get(mm, i, k), get(mm, k, j))
    if (distanceLike.lessThan( newDistance,get(mm, i, j)))
      put(mm, i, j, newDistance)
    else mm
  }
}

class Matrix_Map_Tuple_Like[D] extends Matrix[D, Map[(Int, Int), D]] {
  type MM = Map[(Int, Int), D]
  def makeRaw(numberOfLocations: Int, value: D) = { val locs = 0 to numberOfLocations - 1; (for { l1 <- locs; l2 <- locs } yield ((l1, l2) -> value)).toMap }
  def get(mm: MM, l1: Int, l2: Int) = mm((l1, l2))
  def put(g: MM, l1: Int, l2: Int, d: D) = g + ((l1, l2) -> d)
}

class Matrix_HashMap_Tuple_Like[D] extends Matrix[D, HashMap[(Int, Int), D]] {
  type MM = HashMap[(Int, Int), D]
  def makeRaw(numberOfLocations: Int, value: D) = {
    val result: MM = new HashMap(numberOfLocations)
    val locs = 0 to numberOfLocations - 1;
    for { l1 <- locs; l2 <- locs } put(result, l1, l2, value);
    result
  }
  def get(g: MM, l1: Int, l2: Int) = g.get((l1, l2))
  def put(g: MM, l1: Int, l2: Int, d: D) = { g.put((l1, l2), d); g }
}

class Matrix_Map_Map_Like[D] extends Matrix[D, Map[Int, Map[Int, D]]] {
  type MM = Map[Int, Map[Int, D]]
  def makeRaw(numberOfLocations: Int, value: D): MM = {
    val locs = 0 to numberOfLocations - 1;
    locs.foldLeft(Map[Int, Map[Int, D]]()) { (acc, l1) => locs.foldLeft(acc)((map, l2) => put(map, l1, l2, value)) }
  }

  def get(g: MM, l1: Int, l2: Int) = g.get(l1).get(l2)
  def put(g: MM, l1: Int, l2: Int, d: D) = {
    val oldM = g.getOrElse(l1, Map())
    g + (l1 -> (oldM + (l2 -> d)))

  }
}

class Matrix_HashMap_HashMap_Like[D] extends Matrix[D, HashMap[Int, HashMap[Int, D]]] {
  type MM = HashMap[Int, HashMap[Int, D]]
  def makeRaw(numberOfLocations: Int, value: D): MM = {
    val locs = 0 to numberOfLocations - 1;
    val g: MM = new HashMap(numberOfLocations);
    for (l <- locs) {
      val m = new HashMap[Int, D](numberOfLocations)
      for (l2 <- locs) m.put(l2, value)
      g.put(l, m)
    }
    g
  }
  def get(g: MM, l1: Int, l2: Int) = g.get(l1).get(l2)
  def put(g: MM, l1: Int, l2: Int, d: D) = { g.get(l1).put(l2, d); g }
}

class Matrix_Array_Array_Like[D: Distance] extends Matrix[D, Array[Array[D]]] {
  type MM = Array[Array[D]]
  def makeRaw(numberOfLocations: Int, value: D): MM = implicitly[Distance[D]].makeArrayArray(numberOfLocations, value)
  def get(g: MM, l1: Int, l2: Int) = g(l1)(l2)
  def put(g: MM, l1: Int, l2: Int, d: D) = { g(l1)(l2) = d; g }
}

class Matrix_Array_Like[D: Distance] extends Matrix[D, ArrayOfLocationsAndSize[D]] {
  type MM = ArrayOfLocationsAndSize[D]
  def makeRaw(numberOfLocations: Int, value: D): MM = new ArrayOfLocationsAndSize(numberOfLocations, value);
  def get(g: MM, l1: Int, l2: Int) = g.get(l1, l2)
  def put(g: MM, l1: Int, l2: Int, d: D) = { g.put(l1, l2, d); g }
}

class ArrayOfLocationsAndSize[D: Distance](val locationsSize: Int, initialValue: D) {
  val hs = implicitly[Distance[D]].makeArray(locationsSize * locationsSize, initialValue)
  def get(i1: Int, i2: Int) = hs(i1 * locationsSize + i2)
  def put(i1: Int, i2: Int, d: D) = hs(i1 * locationsSize + i2) = d
}

object Matrix {
  implicit def asMatrix_Map_Tuple_Like[D: Distance] = new Matrix_Map_Tuple_Like[D]
  implicit def asMatrix_HashMap_Tuple_Like[D: Distance] = new Matrix_HashMap_Tuple_Like[D]
  implicit def asMatrix_Int_Map_Map_Like[D: Distance] = new Matrix_Map_Map_Like[D]
  implicit def asMatrix_Int_HashMap_Tuple_Like[D: Distance] = new Matrix_HashMap_HashMap_Like[D]
  implicit def asMatrix_Array_Like[D: Distance] = new Matrix_Array_Like[D]
  implicit def asMatrix_Array_Array_Like[D: Distance] = new Matrix_Array_Array_Like[D]

  implicit object MatrixJavaShortArrayAndLengthLike extends Matrix[Short, ShortArrayAndLength] {
    type Matrix = ShortArrayAndLength
    def makeRaw(numberOfLocations: Int, value: Short): Matrix = { new ShortArrayAndLength(numberOfLocations, value) }
    def get(g: Matrix, l1: Int, l2: Int) = g.get(l1, l2)
    def put(g: Matrix, l1: Int, l2: Int, d: Short) = { g.put(l1, l2, d); g }
  }

  implicit object MatrixJavaIntArrayAndLengthLike extends Matrix[Int, IntArrayAndLength] {
    type Matrix = IntArrayAndLength
    def makeRaw(numberOfLocations: Int, value: Int): Matrix = { new IntArrayAndLength(numberOfLocations, value) }
    def get(g: Matrix, l1: Int, l2: Int) = g.get(l1, l2)
    def put(g: Matrix, l1: Int, l2: Int, d: Int) = { g.put(l1, l2, d); g }
  }

}


