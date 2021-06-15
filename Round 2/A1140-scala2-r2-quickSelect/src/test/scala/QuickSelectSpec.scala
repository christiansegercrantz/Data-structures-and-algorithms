// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import scala.util.Random

class QuickSelectSpec extends AnyFlatSpec with Matchers {
  "Your quickselect implementation" should "compute correct results on some selected inputs" in {
    val tests = List(Seq(1)
                     ,Seq(1,1,1,1)
                     ,Seq(1,2,3)
                     ,Seq(3,2,1)
                   )
    for(test <- tests; k <- 0 until test.length) {
      val result = quickSelect.find(test, k)
      val correct = test.sorted.apply(k)
      withClue(s"On  quickSelect.find($test, $k): ") {
        result should be (correct)
      }
    }
  }

  it should "compute correct results on some random inputs" in {
    val n = 100
    val maxValue = 1000
    val nofTests = 1000
    val rand = new Random()
    for(testNum <- 1 to nofTests) {
      val test = Random.shuffle(Seq.fill[Int](n)(rand.nextInt(maxValue)))
      val k = rand.nextInt(n)
      val result = quickSelect.find(test, k)
      val correct = test.sorted.apply(k)
      withClue(s"On  quickSelect.find($test, $k): ") {
        result should be (correct)
      }
    }
  }

  it should "be twice as fast as the approach based on applying scala.util.Arrays.sort on sequences of 1000000 integers" in {
    val rand = new Random()
    val n = 1000000
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests")
    for(testNum <- 1 to nofTests) {
      println(" Test "+testNum)
      val test = Random.shuffle(IndexedSeq.fill[Int](n)(rand.nextInt()))
      val k = rand.nextInt(n)
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = test.toArray
        java.util.Arrays.sort(tmp)
        tmp(k)
      }
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {
        quickSelect.find(test, k)
      }
      valueQuickSelect should be (valueArraysSort)
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    val speedup = avgArraysSort / avgQuickSelect
    println(f" Speedup: $speedup%.2f")
    speedup should be >= (2.0)
  }

  it should "be as fast as the approach based on applying scala.util.Arrays.sort on almost-sorted sequences of 1000000 integers" in {
    val rand = new Random()
    val n = 1000000
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests on almost-sorted sequences")
    for(testNum <- 1 to nofTests) {
      println(" Test "+testNum)
      val base = rand.nextInt(2*n)-n
      val s1 = Array.tabulate[Int](n)(j => j + base)
      def swap(i: Int, j: Int): Unit = {
        val tmp = s1(i)
        s1(i) = s1(j)
        s1(j) = tmp
      }
      // Swap some random pairs
      for(f <- 1 to n/100) {
        swap(rand.nextInt(n), rand.nextInt(n))
      }
      val test = s1.toIndexedSeq
      val k = rand.nextInt(n)
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = test.toArray
        java.util.Arrays.sort(tmp)
        tmp(k)
      }
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {
        quickSelect.find(test, k)
      }
      valueQuickSelect should be (valueArraysSort)
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    val speedup = avgArraysSort / avgQuickSelect
    println(f" Speedup: $speedup%.2f")
    speedup should be >= (1.0)
  }

  it should "be as fast as the approach based on applying scala.util.Arrays.sort on sequences of 1000000 small integers (=> lots of same values in the array)" in {
    val rand = new Random()
    val n = 1000000
    val maxValue = 21
    var cumuQuickSelect = 0.0
    var cumuArraysSort = 0.0
    val nofTests = 10
    println("Running performance tests on sequences with few values only")
    for(testNum <- 1 to nofTests) {
      println(" Test "+testNum)
      val test = Random.shuffle(IndexedSeq.fill[Int](n)(rand.nextInt(maxValue)))
      val k = rand.nextInt(n)
      val (valueArraysSort, timeArraysSort) = timer.measureCpuTime {
        val tmp = test.toArray
        java.util.Arrays.sort(tmp)
        tmp(k)
      }
      val (valueQuickSelect,timeQuickSelect) = timer.measureCpuTime {
        quickSelect.find(test, k)
      }
      valueQuickSelect should be (valueArraysSort)
      println(f"  quickSelect vs Arrays.sort: $timeQuickSelect%.3g vs $timeArraysSort%.3g")
      cumuQuickSelect += timeQuickSelect
      cumuArraysSort += timeArraysSort
    }
    val avgQuickSelect = cumuQuickSelect / nofTests
    val avgArraysSort = cumuArraysSort / nofTests
    println(f" Average: $avgQuickSelect%.3g vs $avgArraysSort%.3g")
    val speedup = avgArraysSort / avgQuickSelect
    println(f" Speedup: $speedup%.2f")
    speedup should be >= (1.0)
  }
}
