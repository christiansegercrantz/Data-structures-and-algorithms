// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package avl

/**
 * An ordered map class, implemented with AVL trees.
 */
class TreeMap[Key, Value](implicit ord: Ordering[Key]) {
  /*
   * The root of the tree.
   * If the tree is empty, then this should be null.
   * In reality, this would be a private member but we keep it public
   * for the sake of the testing and grading processes.
   */
  var root: Node[Key, Value] = null

  /*
   * In order to have constant time size queries,
   * we keep count of how many keys the tree currently has.
   * Remember to update this when you insert/remove keys.
   */
  private var _nofKeys = 0
  /** The size of the tree */
  def size: Int = _nofKeys
  /** Is the tree empty? */
  def isEmpty = (_nofKeys == 0)

  def leftRotate( currentNode: Node[Key,Value]): Unit = {
    val rightNode = currentNode.right
    if(currentNode != root){
      if(currentNode.parent.right == currentNode){
        currentNode.parent.right = rightNode
      } else {
        currentNode.parent.left = rightNode
      }
    } else{
      root = rightNode
    }
    if(rightNode.hasLeft){
      currentNode.right = rightNode.left
    } else{
      currentNode.right = null
    }
    rightNode.left = currentNode
    currentNode.updateHeight()
    rightNode.updateHeight()
  }

  def rightRotate( currentNode: Node[Key,Value]): Unit = {
    val leftNode = currentNode.left
    if(currentNode != root){
      if(currentNode.parent.right == currentNode){
        currentNode.parent.right_=(leftNode)
      } else {
        currentNode.parent.left_=(leftNode)
      }
    } else {
      root = leftNode
    }
    if(leftNode.hasRight){
      currentNode.left = leftNode.right
    } else {
      currentNode.left = null
    }
    leftNode.right = currentNode
    currentNode.updateHeight()
    leftNode.updateHeight()
  }

  def rebalanceNode( currentNode: Node[Key,Value]): Unit = {
    currentNode.updateHeight()
    val bal = currentNode.balance
    currentNode.balance match {
      case x if x < -1 => {
        if(currentNode.left.balance > 0) {
            leftRotate(currentNode.left)
        }
        rightRotate(currentNode)
      }
      case x if x > 1 => {
        if(currentNode.right.balance < 0) {
            rightRotate(currentNode.right)
        }
        leftRotate(currentNode)
      }
      case _ => {}
      if(currentNode.hasParent){
          rebalanceNode(currentNode.parent)
      }

    }
  }
  
  /**
   * Insert the (key,value) mapping in the tree.
   * If the key already has a value, replace it with the new one.
   * Returns the old value of the key or None if it did not have one.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def insert(key: Key, value: Value): Option[Value] = {
    if(root == null){
        root = new Node(key,value)
        _nofKeys += 1
        return None
    }
    def transverseTree(nodeToInsert: Node[Key, Value], currentNode: Node[Key, Value]): Option[Value] = {
      if(nodeToInsert.key == 22){
        1+1
      }
      var comp = ord.compare(nodeToInsert.key, currentNode.key)
      ord.compare(nodeToInsert.key, currentNode.key) match{
        case 0 => {
          val oldValue = currentNode.value
          currentNode.value = nodeToInsert.value
          return Some(oldValue)
        }
        case -1 => {
          if(currentNode.left == null){
            currentNode.left = nodeToInsert
            _nofKeys += 1
            rebalanceNode(nodeToInsert)
            return None
          }
          transverseTree(nodeToInsert, currentNode.left)
        }
        case 1 => {
          if(currentNode.right == null){
            currentNode.right = nodeToInsert
            _nofKeys += 1
            rebalanceNode(nodeToInsert)
            return None
          }
          transverseTree(nodeToInsert, currentNode.right)
        }
      }
    }

    transverseTree(new Node(key,value), root)
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
      if( currentNode.key == 7 && root.key == 7 && currentNode.hasLeft && currentNode.left.key == 3 && currentNode.hasRight && currentNode.right.key == 13){
        1+1
      }
      ord.compare(currentNode.key, key) match {
        case 0 => {
          _nofKeys -= 1
          val ret: Value = currentNode.value
          if(currentNode.left == null){
            if(currentNode.right == null){ 
              if(currentNode.hasParent){
                val formerParent = currentNode.parent
                substWith(currentNode, null)
                rebalanceNode(formerParent)
              } else{
                substWith(currentNode, null)
              }
              return(Some(ret))
            }
            val formerRight = currentNode.right
            substWith(currentNode, currentNode.right)
            rebalanceNode(formerRight)
            return(Some(ret))
          }

          val replacementNode = largestLeft(currentNode.left)
          currentNode.key = replacementNode.key
          currentNode.value = replacementNode.value
          val formerReplNodeParent = replacementNode.parent
          substWith(replacementNode, replacementNode.left)
          rebalanceNode(formerReplNodeParent)
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
   * Check whether the BST property hods in the tree, i.e., for each node
   * the descendants in the left sub-tree should be less than the key, and
   * the descendants in the right sub-tree should be greater than the key.
   * Linear in the size of the tree, only for debuggin and validation purposes.
   */
  def isValidBST: Boolean = {
    def inner(n: Node[Key,Value], lower: Option[Key], upper: Option[Key]): Boolean = {
      if(n == null) true
      else if(lower.nonEmpty && ord.compare(n.key, lower.get) <= 0) false
      else if(upper.nonEmpty && ord.compare(n.key, upper.get) >= 0) false
      else inner(n.left, lower, Some(n.key)) && inner(n.right, Some(n.key), upper)
    }
    inner(root, None, None)
  }

  /**
   * Does the tree have the AVL property, i.e., is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty: Boolean = hasAVLProperty(root)

  /**
   * Does the sub-tree rooted at the node have the AVL property, i.e.,
   * is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty(node: Node[Key,Value]): Boolean = {
    def inner(n: Node[Key,Value]): (Boolean,Int) = {
      if(n == null) (true, -1)
      else {
        val (leftOk, leftHeight) = inner(n.left)
        if(!leftOk) return (false, 0)
        val (rightOk, rightHeight) = inner(n.right)
        if(!rightOk) return (false, 0)
        val balance = rightHeight - leftHeight
        if(!(-1 <= balance && balance <= 1)) return (false, 0)
        (true, 1 + (leftHeight max rightHeight))
      }
    }
    inner(node)._1
  }


  /**
   * Print the tree in a nicely formatted multi-line string.
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
