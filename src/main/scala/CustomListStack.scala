package org.jordanlewis.pfds

object CustomListStack {
  def apply[T] = new CustomListStack[T](NIL)
}

class CustomListStack[T] private (private val l: LIST[T]) extends Stack[T] {
  def isEmpty = l match {
    case NIL => true
    case CONS(_, _) => false
  }

  def cons[U >: T](x: U) = new CustomListStack[U](new CONS[U](x, l))
  def head = l match {
    case NIL => throw new IllegalArgumentException("Fail")
    case CONS(x, _) => x
  }

  def tail = l match {
    case NIL => throw new IllegalArgumentException("Fail")
    case CONS(_, x) => new CustomListStack[T](x)
  }
}
