package org.jordanlewis.pfds

sealed abstract class Tree[+T]
case object TreeLeaf extends Tree[Nothing]
final case class TreeNode[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

trait TreeSet {
  def insert[T <% Ordered[T]](x: T, t: Tree[T]): Tree[T]
  def member[T <% Ordered[T]](x: T, t: Tree[T]): Boolean
}

