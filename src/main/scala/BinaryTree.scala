package org.jordanlewis.pfds

sealed abstract class BinaryTree[+T]
case object BinaryTreeLeaf extends BinaryTree[Nothing]
final case class BinaryTreeNode[T](left: BinaryTree[T], value: T, right: BinaryTree[T]) extends BinaryTree[T]
