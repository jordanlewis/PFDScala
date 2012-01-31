package org.jordanlewis.pfds

object ListStack {
  def apply[T] : ListStack[T] = new ListStack[T](List())
}

class ListStack[T] private (val l: List[T]) extends Stack[T] {
  def isEmpty = l.isEmpty

  def cons[U >: T](x: U) = new ListStack(x::l)
  def head = l.head
  def tail = new ListStack(l.tail)
}
