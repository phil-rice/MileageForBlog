package org.validoc.mileage

abstract class MileageFactory[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) {
  protected val distanceLike = implicitly[Distance[D]]
  import distanceLike._
  import matrixLike._
  def apply(locationSize: Int, edges: Traversable[MileageEdge[D]]): Mileage[D, MM]

  protected def createAndInitialiseMatrix(locationSize: Int, edges: Traversable[MileageEdge[D]]) = {
    val withLocToLocBeingZero = (0 to locationSize - 1).
      foldLeft(matrixLike.makeRaw(locationSize, large))(
        (hs, loc) => put(hs, loc, loc, zero))

    edges.foldLeft(withLocToLocBeingZero) {
      case (mm, MileageEdge(from, to, distance)) =>
        put(put(mm, from, to, distance), to, from, distance)
    }
  }
}

/** D is how the distance is measured, and MM is the mileage matrix representation  */
class MileageFactory1[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) extends MileageFactory[D, MM] {
  import distanceLike._
  import matrixLike._
  def apply(locationSize: Int, edges: Traversable[MileageEdge[D]]) = {
    val locs = 0 to locationSize - 1
    var mm = createAndInitialiseMatrix(locationSize, edges)
    for { k <- locs; i <- locs; j <- locs } {
      val newDistance = add(get(mm, i, k), get(mm, k, j))
      if (lessThan(newDistance, get(mm, i, j)))
        mm = put(mm, i, j, newDistance)
    };
    new Mileage(mm)
  }
}
class Mileage[D: Distance, MM](hs: MM)(implicit matrixLike: Matrix[D, MM]) {
  def apply(source: Int, destination: Int) = matrixLike.get(hs, source, destination)
}

class MileageFactory2[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) extends MileageFactory[D, MM] {
  def apply(locationSize: Int, edges: Traversable[MileageEdge[D]]) = {
    val locs = 0 to locationSize - 1
    var mm = createAndInitialiseMatrix(locationSize, edges)
    for { k <- locs; i <- locs; j <- locs }
      mm = matrixLike.addIfBetter(mm, k, i, j)
    new Mileage[D, MM](mm)
  }
}
class MileageFactory3[D: Distance, MM](implicit matrixLike: Matrix[D, MM]) extends MileageFactory[D, MM] {
  def apply(locationSize: Int, edges: Traversable[MileageEdge[D]]) = {
    var mm = createAndInitialiseMatrix(locationSize, edges)
    var k = 0;
    while (k < locationSize) {
      var i = 0;
      while (i < locationSize) {
        var j = 0;
        while (j < locationSize) {
          mm = matrixLike.addIfBetter(mm, k, i, j)
          j += 1
        }
        i += 1
      }
      k += 1
    }
    new Mileage[D, MM](mm)
  }
}