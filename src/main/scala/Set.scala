package org.jordanlewis.pfds

trait Set[+T] {
  def insert[U >: T](x: U): Set[U]
  def member[U >: T](x: U): Boolean
}
