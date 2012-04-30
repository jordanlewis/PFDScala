package org.jordanlewis.pfds

class UnbalancedTreeSet[T] private (tree: Tree[T]) (implicit val ordering: Ordering[T])
  extends Set[T] {

  def this()(implicit ordering : Ordering[T]) = this(TreeLeaf)(ordering)
  private def newSet(tree: Tree[T]) = new UnbalancedTreeSet[T](tree);

  def insert(x: T) = newSet(UnbalancedTreeSet.insert(x, tree))
  def member(x: T) = UnbalancedTreeSet.member(x, tree)
}

object UnbalancedTreeSet {
  private def insert[T](x: T, t: Tree[T])(implicit ordering: Ordering[T]): Tree[T] = t match {
    case TreeLeaf => TreeNode(TreeLeaf, x, TreeLeaf)
    case tn @ TreeNode(a, y, b) =>
      if (ordering.lt(x, y)) TreeNode(insert(x, a), y, b)
      else if (ordering.lt(y, x)) TreeNode(a, y, insert(x, b))
      else tn
  }
  private def member[T](x: T, t: Tree[T])(implicit ordering: Ordering[T]): Boolean = (x, t) match {
    case (_, TreeLeaf) => false
    case (_, TreeNode(a, y, b)) =>
      if (ordering.lt(x, y)) member(x, a)
      else if (ordering.lt(y, x)) member(x, b)
      else true
  }
}


