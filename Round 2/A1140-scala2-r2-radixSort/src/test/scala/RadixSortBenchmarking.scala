// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

package tests

import org.scalameter.api._
//import org.scalameter.picklers.Implicits._

object RadixSortBenchmarking extends Bench.OfflineReport {
  val sizes: Gen[Int] = Gen.range("size")(50000, 2000000, 50000)

  val nofSamples = 1
  val nofRuns = 11
  val rand = new scala.util.Random()
  var l = new Array[Int](1)
  def initArray(size: Int): Unit = {
    l = Array.fill(size)(rand.nextInt(Int.MaxValue))
  }

  performance of "running time" in {
    measure method "radix-vs-standard" config (
      exec.benchRuns := nofRuns,
      exec.independentSamples := nofSamples
    ) in {
      using(sizes) curve ("radix sort") warmUp {
        initArray(1000000)
        radixSort.lsdRadixSort(l)
      } setUp {
        n => initArray(n)
      } in {
        n => radixSort.lsdRadixSort(l)
      }
      using(sizes) curve ("java.util.Arrays.sort") warmUp {
        initArray(1000000)
        java.util.Arrays.sort(l)
      } setUp {
        n => initArray(n)
      } in {
        n => java.util.Arrays.sort(l)
      }
    }
  }
}
