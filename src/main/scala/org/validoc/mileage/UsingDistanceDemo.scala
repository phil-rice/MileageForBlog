package org.validoc.mileage

object UsingDistanceDemo {

  def isSumOfDistancesLessThen[D: Distance](targetDistance: D)(distances: D*) = 
    implicitly[Distance[D]].lessThan(addDistance(distances:_*), targetDistance)

  def addDistance[D](distances: D*)(implicit distanceLike: Distance[D]) = {
    import distanceLike._
    distances.foldLeft(zero)(add)
  }
  
  def main(args: Array[String]): Unit = {
    println(addDistance(1, 2, 3))
    println(addDistance[Int](1, 2, 3))
    println(addDistance[Short](1, 2, 3))
  }

}