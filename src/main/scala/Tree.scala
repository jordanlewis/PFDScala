package org.jordanlewis.pfds

sealed abstract class Tree[+T]
case object TreeLeaf extends Tree[Nothing]
final case class TreeNode[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]
