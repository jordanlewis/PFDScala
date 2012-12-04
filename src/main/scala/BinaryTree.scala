package org.jordanlewis.pfds

object BinaryTree {
  def complete[T](x: T, d: Int): BinaryTree[T] = d match {
    case 0 => BinaryTreeLeaf
    case _ => {
      val subtree = complete(x, d - 1)
      BinaryTreeNode(subtree, x, subtree)
    }
  }

  def balanced[T](x: T, d: Int)(implicit ordering: Ordering[T]): BinaryTree[T] = d match {
    case 0 => BinaryTreeLeaf
    case 1 => BinaryTreeNode(BinaryTreeLeaf, x, BinaryTreeLeaf)
    case _ => {
      val half = d / 2
      val halftree = create(x, half)
      if (half * 2 == d) BinaryTreeNode(halftree, x, create(x, half - 1))
      else BinaryTreeNode(halftree, x, halftree)
    }
  }
}

sealed abstract class BinaryTree[+T] {
  def size(): Int
}
case object BinaryTreeLeaf extends BinaryTree[Nothing] {
  def size() = 0
}
final case class BinaryTreeNode[T](left: BinaryTree[T], value: T, right: BinaryTree[T]) extends BinaryTree[T] {
  def size() = 1 + left.size + right.size
}
