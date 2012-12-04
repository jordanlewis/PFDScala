package org.jordanlewis.pfds

class ElementAlreadyExistsException extends RuntimeException

class UnbalancedTreeSet[T] private (tree: BinaryTree[T]) (implicit val ordering: Ordering[T])
  extends Set[T] {

  def this()(implicit ordering : Ordering[T]) = this(BinaryTreeLeaf)(ordering)
  def newSet(tree: BinaryTree[T]) = new UnbalancedTreeSet[T](tree);

  def insert(x: T) = try {
    newSet(UnbalancedTreeSet.insert(x, tree, None))
  } catch {
    case _:ElementAlreadyExistsException => this
  }
  def member(x: T) = UnbalancedTreeSet.member(x, tree, None)
}

object UnbalancedTreeSet {
  private def insert[T](x: T, t: BinaryTree[T], c: Option[T])(implicit ordering: Ordering[T]): BinaryTree[T] = (t, c) match {
    case (BinaryTreeLeaf, None) => BinaryTreeNode(BinaryTreeLeaf, x, BinaryTreeLeaf)
    case (BinaryTreeLeaf, Some(d)) =>
      if (ordering.equiv(x, d)) throw new ElementAlreadyExistsException
      else BinaryTreeNode(BinaryTreeLeaf, x, BinaryTreeLeaf)
    case (BinaryTreeNode(a, y, b), _) =>
      if (ordering.lt(x, y)) BinaryTreeNode(insert(x, a, c), y, b)
      else BinaryTreeNode(a, y, insert(x, b, Some(y)))
  }
  private def member[T](x: T, t: BinaryTree[T], c: Option[T])(implicit ordering: Ordering[T]): Boolean = (t, c) match {
    case (BinaryTreeLeaf, None) => false
    case (BinaryTreeLeaf, Some(d)) => ordering.equiv(x, d)
    case (BinaryTreeNode(a, y, b), _) =>
      if (ordering.lt(x, y)) member(x, a, c)
      else member(x, b, Some(y))
  }
}
