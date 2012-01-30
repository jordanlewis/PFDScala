package org.jordanlewis.PFDScala

object ListStack {
  def apply[T] : ListStack[T] = new ListStack[T](List())
}

class ListStack[T] private (val l: List[T]) extends Stack[T] {
  def isEmpty : Boolean = l.isEmpty

  def cons[U >: T](x: U) : Stack[U] = new ListStack(x::l)
  def head : T = l.head
  def tail : Stack[T] = new ListStack(l.tail)
}
