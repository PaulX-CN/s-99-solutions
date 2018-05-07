package binaryTree

object P60 {

  def minHbalNodes(height: Int): Int = {
    if (height <= 1) height
    else minHbalNodes(height - 1) + minHbalNodes(height - 2) + 1
  }

  def maxHbalNodes(height: Int): Int = 2 * height - 1

  def maxHbalHeight(nodes: Int): Int =
    Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

  def minHbalHeight(nodes: Int): Int = {
    if (nodes == 0) 0
    else minHbalHeight(nodes / 2) + 1
  }

  import P55toP59nP61toP66._

  def hbalTreesWithNodes[T](nodes: Int, v: T): List[TreeExtension[T]] = {
    val maxH = maxHbalHeight(nodes)
    val minH = minHbalHeight(nodes)

    (minH to maxH).flatMap(n => TreeExtension.hbalTress(n, v)).filter(_.nodeCount == nodes).toList
  }
}
