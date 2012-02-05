package org.jordanlewis.pfds

object UnbalancedSet {
  def apply[T <% Ordered[T]]() = EmptyUnbalancedSet
}

sealed abstract class UnbalancedSet[+T <% Ordered[T]] {
  def insert[U >: T <% Ordered[U]](x: U): UnbalancedSet[U]
  def member[U >: T <% Ordered[U]](x: U): Boolean
}

case object EmptyUnbalancedSet extends UnbalancedSet[Nothing] {
  def insert[U <% Ordered[U]](x: U) = NonEmptyUnbalancedSet(this, x, this)
  def member[U <% Ordered[U]](x: U) = false
}

final case class NonEmptyUnbalancedSet[+T <% Ordered[T]]
    (left: UnbalancedSet[T], value: T, right: UnbalancedSet[T]) extends UnbalancedSet[T] {
  def insert[U >: T <% Ordered[U]](x: U) =
    if      (x < value) NonEmptyUnbalancedSet(left insert x, value, right)
    else if (x > value) NonEmptyUnbalancedSet(left, value, right insert x)
    else this
  def member[U >: T <% Ordered[U]](x: U) =
    if      (x < value) left member x
    else if (x > value) right member x
    else true
}
