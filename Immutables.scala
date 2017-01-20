import scala.collection.mutable.HashSet
/** A data structure that stores the immutable blocks */
class Immutables {
  /** Store the tuples */
  private var positions = new HashSet[(Int,Int)]()
  /** Find the block this position is in; return (-1,-1) if there is no such block */
  def findBlock(pos: Int): (Int,Int) = {
    for((from,to) <- positions) {
      if(from <=pos && pos<= to) return (from,to)
    }
    return (-1,-1)
  }
  /** Increment the tuple's members
    * pre: pos is not in a block*/
  private def increment(pos: Int, num: Int): Unit = {
    var incremented = new HashSet[(Int,Int)]()
    for(tuple <- positions) {
      if (tuple._1 > pos) {
        var inc = (tuple._1 + num, tuple._2 + num)
        positions = positions - tuple
        incremented = incremented + inc
      }
    }
    positions = positions ++ incremented
  }
  /** Decrement the tuple's members
    * pre: pos is not in a block */
  private def decrement(pos: Int, num: Int): Unit = {
    increment(pos, -num)
  }
  /** Insert a block
    * pre: no member of the block is in another block */
  def insert(block: (Int,Int)): Unit = {
    var num = block._2 - block._1 + 1
    increment(block._1, num)
    positions += block
  }
  /** Remove a block from the list
    * pre: pos is in a block */
  def remove(pos: Int): Unit = {
    var block = findBlock(pos)
    positions -= block
    var num = block._2 - block._1 + 1
    decrement(pos, num)
  }
  def print(): Unit = {
    var s: String = ""
    for(tuple <- positions) s = s + " " + tuple
    System.out.println(s)
  }
}