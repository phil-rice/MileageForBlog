package org.validoc.mileage

import scala.util.Random
import java.util.HashMap
import java.lang.instrument.Instrumentation

object SimpleTiming {
  def durationToString(duration: Long) = f"${duration / 1000.0}%,20.3fus"
  def time[X](title: String, x: => X) = {
    val newStart = System.nanoTime()
    val result = x
    println(f"$title%-30s took ${durationToString(System.nanoTime() - newStart)}")
    result
  }
  def makeSimpleEdges(locationSize: Int) = (0 to locationSize).map(_ => (Random.nextInt(locationSize), Random.nextInt(locationSize)))

  def makeEdges[D: Distance](locationSize: Int) = {
    var result = List[(Int, Int)]()
    while (result.size < locationSize) {
      result = (result ++ makeSimpleEdges(locationSize)).distinct
    }
    val distanceLike = implicitly[Distance[D]]
    result.take(locationSize).map(kv => MileageEdge[D](kv._1, kv._2, distanceLike.random))
  }
  def makeRandomLocations(locationSize: Int, timesToUse: Int) = {
    def randomLocs = (1 to timesToUse).map(_ => Random.nextInt(locationSize))
    val loc1 = randomLocs
    val loc2 = randomLocs
    (loc1, loc2)
  }

  def makeAndUse[D: Distance, MM](locationSize: Int, timesToUse: Int, factory: MileageFactory[D, MM])(implicit graphLike: Matrix[D, MM]) = {
    val edges = makeEdges(locationSize)
    val mileage = factory.apply(locationSize, edges)
    val (loc1, loc2) = makeRandomLocations(locationSize, timesToUse)
    for (i <- 0 to timesToUse - 1)
      mileage(loc1(i), loc2(i)) //doesn't do anything with the result...
  }

  def main(args: Array[String]): Unit = {
    makeAndUse(100, 500000000, new MileageFactory1[Int, Array[Array[Int]]])
  }

}