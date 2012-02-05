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
  override def member(x: T) =
    if      (x < value) left member x
    else if (x > value) right member x
    else true
}


//private sealed abstract class Tree[+T]
//private case object TreeLeaf extends Tree[Nothing]
//private final case class TreeNode[T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]
//
//class UnbalancedSet[T <% Ordered[T]] private (private val t: Tree[T]) extends Set[T] {
//  def insert[T](x: T): Set[T] = t match {
//    case TreeLeaf => new TreeNode(new TreeLeaf[T], x, new TreeLeaf[T])
//    case tn @ TreeNode(a, y, b) =>
//      if (x < y) new TreeNode(a.insert(x), y, b)
//      else if (y < x) new TreeNode(a, y, b.insert(x))
//      else tn
//    }
//  def member[T](x: T): Boolean = (x, t) match {
//    case (x, TreeLeaf) => false
//    case (x, TreeNode(a, y, b)) =>
//      if (x < y) a.member(x)
//      else if (y < x) b.member(x)
//      else true
//    }
//}
//
