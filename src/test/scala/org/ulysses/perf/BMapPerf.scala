package org.ulysses.perf

import org.ulysses.data.BMap
import java.util.Date
import scala.collection.Map
/**
 * User: arjan
 */

object BMapPerf {
  import PerfLib._
  import BMap._

   def main(args: Array[String]) {
     val sample = 2000000
     println("start inserting %s elements in the bmap at %s".format(sample, new Date))
     var bMap = empty[String, String]
     var scalaMap: Map[String, String] = Map[String, String]()
     val bmapOp = "BMap" -> time {
      val indexes = new Array[String](sample)
      for (i <- 0 until indexes.length) {
        indexes(i) = scala.math.random.toString
      }

      var i = 0
      while (i < indexes.length) {
        bMap = bMap.insert(i.toString, "the data")
        i += 1
      }
     }
     println("Finished inserting %s elements in the bmap at %s".format(sample, new Date))
     val scalaMapOp = "ScalaMap" -> time {
      val indexes = new Array[String](sample)
      for (i <- 0 until indexes.length) {
        indexes(i) = scala.math.random.toString
      }

     var j = 0

      while (j < indexes.length) {
        scalaMap = scalaMap + (j.toString -> "the data")
        j += 1
      }
     }
     println("Finished inserting %s elements in the scala map at %s".format(sample, new Date))
     val res1 = bmapOp._2
     val res2 = scalaMapOp._2
     println("Bmap: took %s nanos".format(res1.nanos))
     println("Bmap took %s seconds".format(res1.nanos / 1000000))
     println("ScalaMap: took %s nanos".format(res2.nanos))
     println("ScalaMap: took %s seconds".format(res2.nanos / 1000000))

     scalaMapOp compare bmapOp
   }
}