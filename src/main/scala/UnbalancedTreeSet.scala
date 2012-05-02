package org.jordanlewis.pfds

class UnbalancedTreeSet[T] private (tree: BinaryTree[T]) (implicit val ordering: Ordering[T])
  extends Set[T] {

  def this()(implicit ordering : Ordering[T]) = this(BinaryTreeLeaf)(ordering)
  private def newSet(tree: BinaryTree[T]) = new UnbalancedTreeSet[T](tree);

  def insert(x: T) = newSet(UnbalancedTreeSet.insert(x, tree))
  def member(x: T) = UnbalancedTreeSet.member(x, tree)
}

object UnbalancedTreeSet {
  private def insert[T](x: T, t: BinaryTree[T])(implicit ordering: Ordering[T]): BinaryTree[T] = t match {
    case BinaryTreeLeaf => BinaryTreeNode(BinaryTreeLeaf, x, BinaryTreeLeaf)
    case tn @ BinaryTreeNode(a, y, b) =>
      if (ordering.lt(x, y)) BinaryTreeNode(insert(x, a), y, b)
      else if (ordering.lt(y, x)) BinaryTreeNode(a, y, insert(x, b))
      else tn
  }
  private def member[T](x: T, t: BinaryTree[T])(implicit ordering: Ordering[T]): Boolean = (x, t) match {
    case (_, BinaryTreeLeaf) => false
    case (_, BinaryTreeNode(a, y, b)) =>
      if (ordering.lt(x, y)) member(x, a)
      else if (ordering.lt(y, x)) member(x, b)
      else true
  }
}


