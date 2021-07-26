// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

package object change {
  def count(amount: Int, denominations: Set[Int]): Long = {
    require(amount > 0)
    require(denominations.forall(d => d > 0))
    val m = denominations.size
    val _denominations = denominations.toArray.sorted
    val _change = new Array[Array[Long]](amount+1)
    _change(0) = Array(1.toLong) ++ Array.fill(amount)(0.toLong)
    for(k <- 0 until m){
      val coin = _denominations(k)
      val tempArr = Array(1.toLong) ++ Array.fill(amount)(0.toLong)
      for( i <- 1 to amount){
        val newSum = i - coin
        if(newSum >= 0){
          tempArr(i) +=  tempArr(i-coin)
        } 
        tempArr(i) += _change(k)(i)
      }
      _change(k+1) = tempArr
    }
  _change(m)(amount)
  }
}
