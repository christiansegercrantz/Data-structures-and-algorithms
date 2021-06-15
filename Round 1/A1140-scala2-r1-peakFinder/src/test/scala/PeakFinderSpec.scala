// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import peakFinder._

class PeakFinderSpec extends AnyFlatSpec with Matchers {
  val rand = new scala.util.Random()

  // Some small test cases for evaluating the correctness of the algorithms
  val testCases = List(Array(-2)
                       ,Array(3)
                       ,Array(1,2)
                       ,Array(2,1)
                       ,Array(2,2)
                       ,Array(1,2,3)
                       ,Array(3,2,1)
                       ,Array(1,2,1)
                       ,Array(1,1,1)
                       ,Array(1,-1,1)
                       ,Array(1,-1,-1)
                       ,Array(1,2,3,4,5,6,5)
                       ,Array(1,2,3,4,5,6,5,4)
                       ,Array(4,5,6,5,4,3,2,1,0,-1)
                       ,Array(1,2,3,4,5,6,5,4,3)
                     )

  "The linear-time algorithm" should "work correctly" in {
    for(testCase <- testCases) {
      withClue(s"""On Array(${testCase.mkString(", ")}): """) {
        val index = solveLinear(testCase)
        isPeak(testCase, index) should be (true)
      }
    }
  }

  "The log-time algorithm" should "work correctly" in {
    for(testCase <- testCases) {
      withClue(s"""On Array(${testCase.mkString(", ")}): """) {
        val index = solveLog(testCase)
        isPeak(testCase, index) should be (true)
      }
    }
  }

  /*
   * Running time performance test.
   */
  val speedupTestN = 10000000
  val requiredSpeedup = 100.0

  it should s"be at least $requiredSpeedup times faster than the linear-time algorithm on arrays with $speedupTestN elements and only one peak" in {
    val nofTests = 11
    val n = speedupTestN
    val speedups = scala.collection.mutable.ArrayBuffer[Double]()
    for(t <- 1 to nofTests) {
      val peakIndex = rand.nextInt(n*60/100)+(n*20/100)
      val base = rand.nextInt(2*n)-n
      val a = Array.tabulate[Int](n)(i => base-math.abs(peakIndex-i))
      val (iLinear, tLinear) = timer.measureCpuTime {solveLinear(a) }
      val (iLog, tLog) = timer.measureCpuTime {solveLog(a) }
      val speedup = tLinear / tLog
      println(f"""Running time, linear vs logarithmic: $tLinear%.6f vs $tLog%.6f
  speedup $speedup%.1f""")
      isPeak(a, iLinear) should be (true)
      isPeak(a, iLog) should be (true)
      speedups += speedup
    }
    val sortedSpeedups = speedups.sorted
    val medianSpeedup = sortedSpeedups(nofTests/2)
    println(f"The median of speedups: $medianSpeedup%.2f")
    medianSpeedup should be >= requiredSpeedup
  }

  /*
   * Test for logarithmic amount of comparisons.
   */

  var nofComps = 0
  class CompMeasuringInt(private val v: Int) extends Ordered[CompMeasuringInt] {
    def compare(that: CompMeasuringInt): Int = {
      nofComps += 1
      v.compare(that.v)
    }
  }

  val logUseTestN = 1000000

  it should s"use at most 3*log(n) comparisons on arrays with $logUseTestN elements and only one peak" in {
    val n = logUseTestN
    val nofTests = 10
    for(t <- 1 to nofTests) {
      val peakIndex = rand.nextInt(n*60/100)+(n*20/100)
      val base = rand.nextInt(2*n)-n
      val a = Array.tabulate[CompMeasuringInt](n)(i => new CompMeasuringInt(base-math.abs(peakIndex-i)))
      // Run the linear time reference algorithm
      val iLinear = solveLinear(a)
      isPeak(a, iLinear) should be (true)
      // Run the logarithmic algorithm and record the amount of comparisons
      nofComps = 0
      val iLog = solveLog(a)
      val maxUse = math.ceil(3*math.log(n)/math.log(2)).toInt
      println(s"Log-use test $t: used $nofComps of max allowed $maxUse comparisons")
      isPeak(a, iLog) should be (true)
      nofComps should be <= (maxUse)
    }
  }
}
