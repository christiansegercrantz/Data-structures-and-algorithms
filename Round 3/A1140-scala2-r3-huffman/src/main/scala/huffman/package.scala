// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue

/*
 * NOTE: only modify the methods marked with ???.
 *  In other words, do NOT touch the other methods
 *   (the grading process will use unmodified versions of them).
 */

package object huffman {

  /**
   * Encode the sequence of bytes.
   * Returns a pair consisting of
   * - the encoding of a tree (needed for decoding), and
   * - the encoding of the sequence according to the tree.
   * The results are sequences of bits: for manipulation simplicity,
   * bit sequences are here represented with Vectors of Booleans while
   * a more compact way would be to represent them with Arrays of Bytes or Ints.
   */
  def encode(bytes: Vector[Byte]): (Vector[Boolean],Vector[Boolean]) = {
    // Build the Huffman tree for the input byte sequence
    val tree = buildTree(bytes)
    // Encode the input byte sequence
    val encodedBytes = {
      // Get the encoding table mapping bytes to bit sequences
      val table = getTable(tree)
      // Mutable buffer for building the result
      val result = new ArrayBuffer[Boolean]()
      // Encode each input sequence byte
      for(b <- bytes)
        result ++= table(b)
      // The result is converted to an immutable Vector
      result.toVector
    }
    // Encode the Huffman tree in a bit sequence
    val encodedTree = encodeTree(tree)
    // Return the encoded tree and byte sequence
    (encodedTree, encodedBytes)
  }

  /**
   * Reverse of encode.
   * Input consists of
   * - the encoding of a Huffman tree and
   * - the encoding of a byte sequence obtained with the tree.
   * Returns the byte sequence.
   */
  def decode(encodedTree: Vector[Boolean], encodedBytes: Vector[Boolean]): Vector[Byte] = {
    val tree = decodeTree(encodedTree)
    val result = new ArrayBuffer[Byte]()
    val iter = encodedBytes.iterator
    while(iter.hasNext) {
      // Start from the root node
      var node = tree
      var done = false
      while(!done) {
        node match {
          case Internal(left, right) => {
            // Read the next bit in the encoded byte sequence
            val bit = iter.next()
            // And branch by its value: 0 to the left, 1 to the right
            if(bit) node = right
            else node = left
          }
          case Leaf(byte, _) => {
            // Reached a leaf, output the byte
            result += byte
            done = true
          }
        }
      }
    }
    result.toVector
  }


  /**
   * Build a Huffman tree for the byte sequence.
   */
  def buildTree(bytes: Vector[Byte]): Node = {
    /* Hints:
     * First compute the frequencies (number of occurrences)
     *  of the bytes occurring in the sequence.
     * Then associate each byte with non-zero occurrence with
     *  a leaf node and put the nodes in a priority queue.
     * Finally, build the tree by taking the two nodes with smallest
     *  frequencies from the queue, make a new internal node
     *  having them as the children (make the node with smaller
     *  frequency to be the left child), and insert the new node in
     *  the queue. Repeat until the queue has only one node
     *  (the final tree) left.
     * You can also see the Wikipedia article
     *  https://en.wikipedia.org/wiki/Huffman_coding
     */
    val frequencies = bytes.groupMapReduce(identity)(_ => 1)(_ + _)
    //val freqOrdering = Ordering.by{( x: Node => x.freq)}
    var tree = PriorityQueue[Node]()(Ordering.by((x:Node) => x.freq).reverse)
    frequencies.foreach({case (k,v) => tree.enqueue(Leaf(k,frequencies(k)))})
    
    def buildNode(queue: PriorityQueue[Node]) {
      val left: Node = queue.dequeue()
      val right: Node = queue.dequeue()
      val node: Internal = new Internal(right, left)
      queue.enqueue(node)
    }
    while(tree.length > 1){
      buildNode(tree)
    }
    tree.dequeue()
  }

  /**
   * Get the encoding table from a Huffman tree,
   *  associating each byte occurring in the tree to its bit encoding.
   * The encoding value of a byte in a leaf node is formed
   *  by the path from the root of the tree to the leaf node:
   *  - going to the left child adds a 0 (i.e., false),
   *  - going to the right adds a 1 (i.e., true).
   * See the unit tests for examples and you can also
   *  consult the Wikipedia article
   *  https://en.wikipedia.org/wiki/Huffman_coding
   */
  def getTable(tree: Node): Map[Byte, Vector[Boolean]] = {

    var res =  scala.collection.mutable.Map[Byte, Vector[Boolean]]()

    def setBytes(node: Node, encoding: ArrayBuffer[Boolean]) {
      node match {
        case Internal(left, right) => {
          val leftEncoding = encoding.clone
          val rightEncoding = encoding.clone
          setBytes(left, leftEncoding += false)
          setBytes(right, rightEncoding +=  true)
        }
        case Leaf(symbol, freq) => {
          res += (symbol.toByte -> encoding.toVector)
        }
      }
    }
    
    setBytes(tree, new ArrayBuffer[Boolean]())
    return res.toMap
  }

  /**
   * Encode a Huffman tree in a sequence of bits:
   * - The encoding of an internal node consists of a 0 (i.e., false)
   *   followed by the encoding of the left subtree and
   *    the encoding of the right subtree
   * - The encoding of a leaf node consists of a 1 (i.e., true)
   *   followed by the 8 bits encoding the byte of the leaf
   *   node in the most-significant-bit-first order.
   *   Note: the frequency is not included in the encoding,
   *   it is not needed when decoding a byte sequence based on the tree.
   */
  def encodeTree(tree: Node): Vector[Boolean] = {
    def bitValue(byte: Byte, n: Int) = {
      val pow2 = Math.pow(2, n - 1).toInt
      if((pow2 & byte) == 0) 0
    else 1
    }
    def setBits(node: Node): String = {
       node match{
        case Internal(left, right) => {
          val encoding = "0" + setBits(left) + setBits(right)
          return encoding
        }
        case Leaf(symbol, freq)=> {
          //val encoding = symbol.toBinaryString
          var ret = "1"
          var i = 8
          while( i > 0){
            ret += bitValue(symbol,i)
            i -= 1
          }
          return ret
        }
      }
    }
    var res = setBits(tree)
    res.map(_ == '1').toVector
  }

  /**
   * The reverse of encodeTree.
   * The freq-values of the leaf nodes shall be 0 in the decoded tree.
   */
  def decodeTree(bits: Vector[Boolean]): Node = {
    var completeStr: String = bits.foldLeft("")((acc, cur) => acc + {if(cur) 1 else 0})
    def parseNode(str: String): Node = {
      if( str.head == '0'){
        completeStr = completeStr.drop(1)
        return Internal(parseNode(completeStr), parseNode(completeStr))
      } else{
        val leafStr = str.tail.take(8)
        completeStr = completeStr.drop(9)
        return Leaf(Integer.parseInt(leafStr,2).toByte,0)
        
      }
    }
    parseNode(completeStr)
  }
}

