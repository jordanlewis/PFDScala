package org.jordanlewis.PFDScala

object CustomStack {
  def apply[T] : CustomStack[T] = new CustomStack[T](NIL)
}

class CustomStack[T] private (private val l: LIST[T]) extends Stack[T] {
  def isEmpty : Boolean = l match {
    case NIL => true
    case CONS(_, _) => false
  }

  def cons[U >: T](x: U) : Stack[U] = new CustomStack[U](new CONS[U](x, l))
  def head : T = l match {
    case NIL => throw new IllegalArgumentException("Fail")
    case CONS(x, _) => x
  }

  def tail : Stack[T] = l match {
    case NIL => throw new IllegalArgumentException("Fail")
    case CONS(_, x) => new CustomStack[T](x)
  }
}
