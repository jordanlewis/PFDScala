package org.jordanlewis.pfds

sealed abstract class BinaryTree[+T] {
  def size(): Int
}
case object BinaryTreeLeaf extends BinaryTree[Nothing] {
  def size() = 0
}
final case class BinaryTreeNode[T](left: BinaryTree[T], value: T, right: BinaryTree[T]) extends BinaryTree[T] {
  def size() = 1 + left.size + right.size
}
