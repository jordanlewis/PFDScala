package org.jordanlewis.pfds

sealed abstract class CustomStack[+T]
case object EmptyCustomStack extends CustomStack[Nothing] {
  def isEmpty = true
  def cons[T](x: T) = new NonEmptyCustomStack[T](x, EmptyCustomStack)
  def head = throw new IllegalArgumentException("Can't take head of an empty list")
  def tail = throw new IllegalArgumentException("Can't take head of an empty list")
}
final case class NonEmptyCustomStack[+T](head: T, tail: CustomStack[T]) extends CustomStack[T] {
  def isEmpty = false
  def cons[U >: T](x: U) = new NonEmptyCustomStack[U](x, this)
}

