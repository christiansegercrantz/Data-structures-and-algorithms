// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

package object fractionalKnapsack {
  /**
   * Solves fractional knapsack problem instances.
   * Input
   * - maxAmount is the maximum amount that the container can hold
   * - materials gives the available materials
   * Output is a pair with
   * - the maximum profit we can get
   * - and a sequence of pairs of form (a,m) describing how the maximum
   *   profit can be get, a pair (a,m) stating that we should include
   *   the amount a of material m
   */
  def solve(maxAmount: Double, materials: Seq[Material]): (Double, Seq[(Double, Material)]) = {
    require(maxAmount >= 0.0)
    val n = materials.size
    val _materials = materials.toArray.sortBy(_.valuePerUnit)(Ordering[Double].reverse)
    val sol = collection.mutable.ArrayBuffer[(Double, Material)]()
    var amountLeft = maxAmount
    var profit = 0.0
    for(material <- _materials){
      if(material.amount <= amountLeft){
        profit += material.amount * material.valuePerUnit
        sol += ((material.amount, material))
        amountLeft -= material.amount
      } else if(amountLeft>0.0){
        profit += amountLeft * material.valuePerUnit
        sol += ((amountLeft, material))
        amountLeft -= amountLeft
      }
    }
    (profit, sol.toSeq)
  }
}
