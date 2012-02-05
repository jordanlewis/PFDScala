package org.jordanlewis.pfds

object UnbalancedTreeSet extends TreeSet {
  def apply() = TreeLeaf
  def insert[T <% Ordered[T]](x: T, t: Tree[T]): Tree[T] = t match {
    case TreeLeaf => TreeNode(TreeLeaf, x, TreeLeaf)
    case tn @ TreeNode(a, y, b) =>
      if (x < y) TreeNode(insert(x, a), y, b)
      else if (y < x) TreeNode(a, y, insert(x, b))
      else tn
  }
  def member[T <% Ordered[T]](x: T, t: Tree[T]): Boolean = (x, t) match {
    case (x, TreeLeaf) => false
    case (x, TreeNode(a, y, b)) =>
      if (x < y) member(x, a)
      else if (y < x) member(x, b)
      else true
  }
}

