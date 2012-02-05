package org.jordanlewis.pfds

object UnbalancedTreeSet {
  def apply[T <% Ordered[T]]() = EmptyUnbalancedTreeSet
}

sealed abstract class UnbalancedTreeSet[+T <% Ordered[T]] {
  def insert[U >: T <% Ordered[U]](x: U): UnbalancedTreeSet[U]
  def member[U >: T <% Ordered[U]](x: U): Boolean
}

case object EmptyUnbalancedTreeSet extends UnbalancedTreeSet[Nothing] {
  def insert[U <% Ordered[U]](x: U) = NonEmptyUnbalancedTreeSet(this, x, this)
  def member[U <% Ordered[U]](x: U) = false
}

final case class NonEmptyUnbalancedTreeSet[+T <% Ordered[T]]
    (left: UnbalancedTreeSet[T], value: T, right: UnbalancedTreeSet[T]) extends UnbalancedTreeSet[T] {
  def insert[U >: T <% Ordered[U]](x: U) =
    if      (x < value) NonEmptyUnbalancedTreeSet(left insert x, value, right)
    else if (x > value) NonEmptyUnbalancedTreeSet(left, value, right insert x)
    else this
  def member[U >: T <% Ordered[U]](x: U) =
    if      (x < value) left member x
    else if (x > value) right member x
    else true
}
