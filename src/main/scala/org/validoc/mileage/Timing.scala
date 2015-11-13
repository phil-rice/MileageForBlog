package org.validoc.mileage

import scala.util.Random
import java.util.HashMap
import java.lang.instrument.Instrumentation
trait TimeOperations {
  var globalPrintIt = true

  def time[X](title: String, x: => X) = {
    val newStart = System.nanoTime()
    val result = x
    val duration = System.nanoTime() - newStart
    println(f"$title%-30s took  ${durationToString(duration)}")
    (result, duration)
  }
  def justTIme[X](title: String, x: => X) = {
    val newStart = System.nanoTime()
    val result = x
    val duration = System.nanoTime() - newStart
    println(f"$title%-30s took ${durationToString(duration)}")
    result
  }
  def durationToString(duration: Long) = f"${duration / 1000.0}%,20.3fus"
}

object TimeMakingAndUsing extends TimeOperations {
  //  import Matrix._

  def makeSimpleEdges(locationSize: Int) = (0 to locationSize).map(_ => (Random.nextInt(locationSize), Random.nextInt(locationSize)))

  def makeEdges[D: Distance](locationSize: Int) = {
    var result = List[(Int, Int)]()
    while (result.size < locationSize) {
      result = (result ++ makeSimpleEdges(locationSize)).distinct
    }
    val distanceLike = implicitly[Distance[D]]
    result.take(locationSize).map(kv => MileageEdge[D](kv._1, kv._2, distanceLike.random))
  }

  def timeMaking[D: Distance, MM](locationSize: Int, factory: MileageFactory[D, MM])(implicit graphLike: Matrix[D, MM]) = {
    val edges = makeEdges(locationSize)
    val (mileage, makingDuration) = time(f"Making  locations = $locationSize%-10d", factory.apply(locationSize, edges))
    (edges, mileage, makingDuration)
  }

  def timeUsing[D: Distance, MM](locationSize: Int, mileage: Mileage[D, MM])(implicit graphLike: Matrix[D, MM]) = {
    val size = 50000000
    def randomLocs = (1 to size).map(_ => Random.nextInt(locationSize)).toArray
    val loc1 = randomLocs
    val loc2 = randomLocs
    for (i <- 1 to 10) Runtime.getRuntime.gc()
    time(f"Using: $size%,12d", {
      var i = 0
      while (i < size) {
        mileage(loc1(i), loc2(i))
        i = i + 1
      }
    })
  }

  def averageIgnoringWorst(allTimes: Traversable[(Long, Long)], fn: ((Long, Long)) => Long) = {
    val times = allTimes.map(fn)
    val maxTime = times.max
    val withoutWorst = times.filter(_ != maxTime)
    if (withoutWorst.size == 0) times.head else withoutWorst.sum / withoutWorst.size
  }

  def timeMakingAndUsing[D: Distance, MM](timesToDo: Int)(locationSize: Int, factory: MileageFactory[D, MM])(implicit graphLike: Matrix[D, MM]) = {
    val allTimes = (1 to timesToDo).map { _ =>
      val (edges, mileage, durationMaking) = timeMaking[D, MM](locationSize, factory)
      val (_, durationUsing) = timeUsing[D, MM](locationSize, mileage)
      (durationMaking, durationUsing)
    }
    (averageIgnoringWorst(allTimes, _._1), averageIgnoringWorst(allTimes, _._2))
  }

  def timeSequence[D: Distance, MM](title: String, fn: (Int, MileageFactory[D, MM]) => (Long, Long), times: Int*)(implicit graphLike: Matrix[D, MM]) {
    println

    for (i <- times) {
      for (factory <- List(new MileageFactory1[D, MM], new MileageFactory2[D, MM], new MileageFactory3[D, MM])) {
        for (i <- 1 to 10) Runtime.getRuntime.gc()
        println(s"$title $i ${factory.getClass.getSimpleName}")
        val (making, using) = fn(i, factory)
        println
        println(s"Average: $title $i ${factory.getClass.getSimpleName} ${durationToString(making)}, ${durationToString(using)}")
        println
      }
    }
  }

  def timeShort[D: Distance, MM](title: String, fn: (Int, MileageFactory[D, MM]) => (Long, Long))(implicit graphLike: Matrix[D, MM]) = timeSequence(title, fn, 10, 100, 500)
  def timeLong[D: Distance, MM](title: String, fn: (Int, MileageFactory[D, MM]) => (Long, Long))(implicit graphLike: Matrix[D, MM]) = timeSequence(title, fn, 10, 10, 100, 500, 1000, 2500)

  def main(args: Array[String]): Unit = {
    for (timesToDo <- List(1, 3, 10)) {
      //    for (timesToDo <- List(1, 10, 20)) {
      println(s"------------------- Times To Do is $timesToDo -----------------------")
      timeLong("Array[Array[Int]]]", timeMakingAndUsing[Int, Array[Array[Int]]](timesToDo))
      timeLong("Array[Array[Short]]]", timeMakingAndUsing[Short, Array[Array[Short]]](timesToDo))

      timeLong("IntArrayAndLength", timeMakingAndUsing[Int, IntArrayAndLength](timesToDo))
      timeLong("ShortArrayAndLength", timeMakingAndUsing[Short, ShortArrayAndLength](timesToDo))

      timeLong("ArrayOfLocationsAndSize[Int]]", timeMakingAndUsing[Int, ArrayOfLocationsAndSize[Int]](timesToDo))
      timeLong("ArrayOfLocationsAndSize[Short]]", timeMakingAndUsing[Short, ArrayOfLocationsAndSize[Short]](timesToDo))

      timeLong("ShortArrayAndLength", timeMakingAndUsing[Short, ShortArrayAndLength](timesToDo))

      timeShort("HashMap[(Int, Int), Int]]", timeMakingAndUsing[Int, HashMap[(Int, Int), Int]](timesToDo))
      timeShort("HashMap[Int, HashMap[Int, Int]]]", timeMakingAndUsing[Int, HashMap[Int, HashMap[Int, Int]]](timesToDo))

      timeShort(" Map[Int, Map[Int, Int]]]", timeMakingAndUsing[Int, Map[Int, Map[Int, Int]]](timesToDo))

      timeShort("Map[(Int, Int), Int]]", timeMakingAndUsing[Int, Map[(Int, Int), Int]](timesToDo))
    }
  }

}