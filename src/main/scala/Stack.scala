package org.jordanlewis.PFDScala

trait Stack[+T] {
  def isEmpty : Boolean

  def cons[U >: T](x: U) : Stack[U]
  def head : T
  def tail : Stack[T]
}
