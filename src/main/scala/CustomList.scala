package org.jordanlewis.pfds

sealed abstract class LIST[+T]
case object NIL extends LIST[Nothing]
final case class CONS[T](head: T, tail: LIST[T]) extends LIST[T]
