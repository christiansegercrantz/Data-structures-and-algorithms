// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package nutsAndBolts
import scala.reflect.ClassTag

object matcher {
  /**
   * Find matching pairs for the nuts and bolts.
   * A straightforward, quadratic-time algorithm given
   * for reference when measuring performance.
   */
  def slow[NutType <: Nut : ClassTag, BoltType <: Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    val result = scala.collection.mutable.ArrayBuffer[(NutType,BoltType)]()
    val unmatchedBolts = bolts.toArray
    var nofUnmatchedBolts = unmatchedBolts.length
    var i = 0
    while(i < nuts.length && nofUnmatchedBolts > 0) {
      val nut = nuts(i)
      var boltFound = false
      var j = 0
      while(j < nofUnmatchedBolts && boltFound == false) {
        val bolt = unmatchedBolts(j)
        if(nut.compare(bolt) == 0) {
          boltFound = true
          // Found a matching bolt, append the pair to the result
          result += ((nut, bolt))
          // (Pseudo)remove the bolt from the unmatched array by
          // moving it to the end of the current subarray and
          // shrinking the subarray
          nofUnmatchedBolts -= 1
          assert(nofUnmatchedBolts >= 0 && j <= nofUnmatchedBolts)
          unmatchedBolts(j) = unmatchedBolts(nofUnmatchedBolts)
          unmatchedBolts(nofUnmatchedBolts) = bolt
        } else
          j += 1
      }
      i += 1
    }
    result.toIndexedSeq
  }


  /**
   * The faster algorithm based on a variant of quicksort.
   */
  def fast[NutType <: Nut : ClassTag, BoltType <: Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    val result = scala.collection.mutable.ArrayBuffer[(NutType,BoltType)]()

    def findPairs(nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): Unit = {
      val unmatchedNuts = nuts.toArray
      val unmatchedBolts = bolts.toArray
      if(unmatchedNuts.length == 0 || unmatchedBolts.length == 0){
        return 
      }
      var pivotNut = nuts(0)
      var groupedBolts: Map[Int, Seq[BoltType]] = bolts.groupBy(_.compare(pivotNut))
      var pivotBolt = groupedBolts.get(0).get(0)
      var groupedNuts: Map[Int, Seq[NutType]] = nuts.groupBy(_.compare(pivotBolt))
      var sameSize = groupedNuts(0).zip(groupedBolts(0))
      sameSize.foreach(result += _)
      if(groupedNuts.get(-1) != None){
        findPairs(groupedNuts(-1).toIndexedSeq, groupedBolts(-1).toIndexedSeq)
      }
      if(groupedNuts.get(1) != None){
        findPairs(groupedNuts(1).toIndexedSeq, groupedBolts(1).toIndexedSeq)
      }
    }
    findPairs(nuts, bolts)
    return result.toIndexedSeq
  }
}
