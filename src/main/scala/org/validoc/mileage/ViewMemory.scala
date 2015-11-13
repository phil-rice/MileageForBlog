package org.validoc.mileage
//
//import objectexplorer.MemoryMeasurer
//
//object ViewMemory {
//
//  type D = Number
//  type ResultAsSimpleMap = Map[(Int, Int), D]
//  type ResultAsMapOfMaps = Map[Int, Map[Int, D]]
//  type ResultAsArrayOfArrays = Array[Array[D]]
//
//  def main(args: Array[String]): Unit = {
//    println(MemoryMeasurer.measureBytes("hello"))
//    println(MemoryMeasurer.measureBytes(new Array[Long](50)))
//        
//    println(MemoryMeasurer.measureBytes(new ShortArrayAndLength(100, 0)))
//    println(MemoryMeasurer.measureBytes(new IntArrayAndLength(100, 0)))
//    println(MemoryMeasurer.measureBytes(new ArrayOfLocationsAndSize[Short](100, 0)))
//  }
//
//}