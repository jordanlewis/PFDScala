package org.jordanlewis.pfds

object Suffixes {
  def apply[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil::Nil
    case x::xs => (x::xs)::Suffixes(xs)
  }
}

object CustomSuffixes {
  def apply[A](xs: LIST[A]): LIST[LIST[A]] = xs match {
    case NIL => new CONS(NIL, NIL)
    case CONS(x, xs) => new CONS(new CONS(x, xs), CustomSuffixes(xs))
  }
}
