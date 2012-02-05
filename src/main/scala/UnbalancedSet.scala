package org.jordanlewis.pfds

object UnbalancedSet {
  def apply[T <% Ordered[T]]() = EmptyUnbalancedSet[T]
}

sealed abstract class UnbalancedSet[T <% Ordered[T]] extends Set[T] {
  def insert(x: T): UnbalancedSet[T] = NonEmptyUnbalancedSet[T](UnbalancedSet[T], x, UnbalancedSet[T])
  def member(x: T): Boolean
}

final case class EmptyUnbalancedSet[T <% Ordered[T]]() extends UnbalancedSet[T] {
  override def insert(x: T) = NonEmptyUnbalancedSet(UnbalancedSet[T], x, UnbalancedSet[T])
  def member(x: T) = false
}

final case class NonEmptyUnbalancedSet[T <% Ordered[T]]
    (left: UnbalancedSet[T], value: T, right: UnbalancedSet[T]) extends UnbalancedSet[T] {
  override def insert(x: T) =
    if      (x < value) NonEmptyUnbalancedSet(left insert x, value, right)
    else if (x > value) NonEmptyUnbalancedSet(left, value, right insert x)
    else this
  def member(x: T) =
    if      (x < value) left member x
    else if (x > value) right member x
    else true
}


sealed abstract class Tree[+T]
case object TreeLeaf extends Tree[Nothing]
final case class TreeNode[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

class UnbalancedTreeSet[T <% Ordered[T]] private (val t: Tree[T]) extends Set[T] {
  type t
  def apply() = TreeLeaf
  def insert(x: T): UnbalancedTreeSet[T] = t match {
    case TreeLeaf => new UnbalancedTreeSet(TreeNode(TreeLeaf, x, TreeLeaf))
    case tn @ TreeNode(a, y, b) =>
      if (x < y) new UnbalancedTreeSet(TreeNode(new UnbalancedTreeSet(a).insert(x).t, y, b))
      else if (y < x) new UnbalancedTreeSet(TreeNode(a, y, new UnbalancedTreeSet(b).insert(x).t))
      else new UnbalancedTreeSet(tn)
    }
  def member(x: T): Boolean = (x, t) match {
    case (x, TreeLeaf) => false
    case (x, TreeNode(a, y, b)) =>
      if (x < y) new UnbalancedTreeSet(a).member(x)
      else if (y < x) new UnbalancedTreeSet(b).member(x)
      else true
    }
}
