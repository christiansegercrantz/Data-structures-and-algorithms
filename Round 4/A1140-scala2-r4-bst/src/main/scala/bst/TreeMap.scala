// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package bst

import scala.reflect.ClassTag

/**
 * An ordered map class, implemented with unbalanced BSTs.
 */
class TreeMap[Key : ClassTag, Value](implicit ord: Ordering[Key]) {
  /*
   * The root of the tree.
   * If the tree is empty, then this should be null.
   * In reality, this would be a private member but we keep it public
   * for the sake of the testing and grading processes.
   */
  var root: Node[Key, Value] = null

  /*
   * In order to have constant time "size" and "isEmpty" queries,
   * we keep count of how many keys the tree currently has.
   * Remember to update this when you insert/remove keys.
   */
  private var _nofKeys = 0
  /** The size of the tree */
  def size: Int = _nofKeys
  /** Is the tree empty? */
  def isEmpty = (_nofKeys == 0)

  /**
   * Insert the (key,value) mapping in the tree.
   * If the key already has a value, replace it with the new one.
   * Returns the old value of the key or None if it did not have one.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def insert(key: Key, value: Value): Option[Value] = {

    val nodeToInsert = new Node(key,value)
    if(root == null){
        root = nodeToInsert
        _nofKeys += 1
        return None
    }
    def transverseTree(nodeToInsert: Node[Key, Value], currentNode: Node[Key, Value]): Option[Value] = {
      var comp = ord.compare(nodeToInsert.key, currentNode.key)
      ord.compare(nodeToInsert.key, currentNode.key) match{
        case 0 => {
          val oldValue = currentNode.value
          currentNode.value = nodeToInsert.value
          //_nofKeys += 1
          return Some(oldValue)
        }
        case -1 => {
          if(currentNode.left == null){
            currentNode.left = nodeToInsert
            _nofKeys += 1
            return None
          }
          transverseTree(nodeToInsert, currentNode.left)
        }
        case 1 => {
          if(currentNode.right == null){
            currentNode.right = nodeToInsert
            _nofKeys += 1
            return None
          }
          transverseTree(nodeToInsert, currentNode.right)
        }
      }
    }

    transverseTree(nodeToInsert, root)
    
  }

  /**
   * Get the value of the key, or None if the key does not have a value.
   * Should work in time O(h), where h is the height of the tree.
   */
  def get(key: Key): Option[Value] = {

    def inner (currentNode: Node[Key,Value]): Option[Value] = {
      if(currentNode == null){
        return None
      }
      ord.compare(key, currentNode.key) match{
        case 0 =>{
          return Some(currentNode.value)
        }
        case 1 => {
          inner(currentNode.right)
        }
        case -1 => {
          inner(currentNode.left)
        }
      }
    }

    inner(root)
  }

  /*
   * Return the smallest key in the treemap, or None if the tree is empty.
   * Should work in time O(h), where h is the height of the tree.
   */
  def min: Option[Key] = {
    if(root == null){
      return None
    }
    def inner(currentNode: Node[Key, Value]): Option[Key] = {
      if(currentNode.left == null){
        return Some(currentNode.key)
      } else{
        inner(currentNode.left)
      }
    }
    inner(root)
  }

  /*
   * Return the smallest key in the treemap that is equal to or greater than
   * the argument key (or None if the tree is empty or all its keys are
   * less than the argument key).
   * Should work in time O(h), where h is the height of the tree.
   */
  def ceiling(key: Key): Option[Key] = {
    if(root == null ){
      return None
    }
    def inner(currentNode: Node[Key, Value]): Option[Key] = {
      ord.compare(currentNode.key, key) match {
        case -1 => {
          if( currentNode.right == null){
            if(currentNode == root){
              return None
            }
            else{
              return upwards(currentNode.parent)
            }
          }
          inner(currentNode.right)
        }
        case 0 => {
            return Some(currentNode.key)
        }
        case 1 => {
          if( currentNode.left == null){
            return Some(currentNode.key)
          }
          inner(currentNode.left)
        }
      }
    }
    def upwards(currentNode: Node[Key, Value]): Option[Key] = {
      ord.compare(currentNode.key, key) match {
        case -1 => {
          if(currentNode.parent == null){
            return None
          }
          upwards(currentNode.parent)
        }
        case _ => {
          return Some(currentNode.key)
        }
      }
    }
    inner(root)
  }


  /*
   * Return all the keys in the tree in an array, sorted in ascending order.
   */
  def toArray: Array[Key] = {
    val result = new Array[Key](_nofKeys)
    var i = 0
    def inner(node: Node[Key,Value]): Unit = {
      if(node.hasLeft) inner(node.left)
      result(i) = node.key
      i += 1
      if(node.hasRight) inner(node.right)
    }
    inner(root)
    assert(i == _nofKeys)
    result
  }



  /*
   * An internal helper function.
   * Substitutes the node n1 with the node n2.
   * The node n1 must not be null and it (with all its descendants)
   * will be effectively deleted from the tree.
   * The node n2 can be null.
   */
  private def substWith(n1: Node[Key,Value], n2: Node[Key,Value]): Unit = {
    require(n1 != null)
    // Remove the connection from the previous parent of n2
    if(n2 != null && n2.hasParent) {
      if(n2.parent.left == n2) n2.parent.left = null
      else {assert(n2.parent.right == n2); n2.parent.right = null }
    }
    // Make n2 to substitute n1
    if(n1 == root) {root = n2 }
    else {
      if(n1.parent.left == n1) n1.parent.left = n2
      else {assert(n1.parent.right == n1); n1.parent.right = n2 }
    }
  }

  /*
   * Remove the key from the treemap.
   * Do nothing if the key is not in the treemap.
   * Return the old value of the key or None if the key was not in the treemap.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def remove(key: Key): Option[Value] = {
    
    def inner(currentNode: Node[Key, Value]): Option[Value] = {
      ord.compare(currentNode.key, key) match {
        case 0 => {
          _nofKeys -= 1
          val ret: Value = currentNode.value
          if(currentNode.left == null){
            if(currentNode.right == null){ 
              substWith(currentNode, null)
              return(Some(ret))
            }
            substWith(currentNode, currentNode.right)
            return(Some(ret))
          }
          val replacementNode = largestLeft(currentNode.left)
          currentNode.key = replacementNode.key
          currentNode.value = replacementNode.value
          substWith(replacementNode, replacementNode.left)
          return(Some(ret))
        }
        case 1 => {
          if(currentNode.left == null){
            return None
          }
          inner(currentNode.left)
        }
        case -1 => {
          if(currentNode.right == null){
            return None
          }
          inner(currentNode.right)
        }
      }
    }
    def largestLeft (currentNode: Node[Key, Value]): Node[Key, Value] = {
      if( currentNode.right != null){
        return largestLeft(currentNode.right)
      }
      return currentNode
    }
    inner(root)
  }


  /**
   * Check whether the BST property holds in the tree, i.e., for each node
   * the descendants in the left sub-tree should be less than the key, and
   * the descendants in the right sub-tree should be greater than the key.
   * Linear in the size of the tree, only for debuggin and validation purposes.
   */
  def isValidBST(): Boolean = {
    def inner(n: Node[Key,Value], lower: Option[Key], upper: Option[Key]): Boolean = {
      if(n == null) true
      else if(lower.nonEmpty && ord.compare(n.key, lower.get) <= 0) false
      else if(upper.nonEmpty && ord.compare(n.key, upper.get) >= 0) false
      else inner(n.left, lower, Some(n.key)) && inner(n.right, Some(n.key), upper)
    }
    inner(root, None, None)
  }



  /**
   * Print the tree in a nicely formatted multi-line string.
   * Can be used for debugging ;)
   */
  def prettyString: String = prettyString(root)

  def prettyString(subTreeRoot: Node[Key,Value]): String = {
    val s = new scala.collection.mutable.StringBuilder()
    def sep = " "
    def inner(node: Node[Key,Value], indent: String): Unit = {
      s ++= indent
      if(node == null) {
        s ++= "null\n"
      } else {
        s ++= s"key=${node.key} -> value=${node.value}\n"
        inner(node.left, indent+sep)
        inner(node.right, indent+sep)
      }
    }
    inner(subTreeRoot, "")
    s.toString
  }
}
