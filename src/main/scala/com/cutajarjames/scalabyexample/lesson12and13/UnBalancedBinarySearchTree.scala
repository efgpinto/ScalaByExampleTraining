package com.cutajarjames.scalabyexample.lesson12and13

class UnBalancedBinarySearchTree[K, V](root: Node[K, V], ord: Ordering[K]) extends BinarySearchTree[K, V] {

  override def search(key: K): Option[V] = search(Some(root), key)

  private def search(node: Option[Node[K, V]], key: K): Option[V] = node match {
    case None => None
    case Some(n) if n.key == key => Some(n.value)
    case Some(n) if ord.gt(key, n.key) => search(n.right, key)
    case Some(n) => search(n.left, key)
  }

  override def insert(key: K, value: V): BinarySearchTree[K, V] = {
    val rootNode = insert(key, value, Some(root))
    new UnBalancedBinarySearchTree[K, V](rootNode, ord)
  }

  private def insert(key: K, value: V, node: Option[Node[K, V]]): Node[K, V] = node match {
    case None => Node(key, value)
    case Some(n) if n.key == key => Node(key, value, n.left, n.right)
    case Some(n) if ord.gt(key, n.key) =>
      val newRight = insert(key, value, n.right)
      n.copy(right = Some(newRight))
    case Some(n) =>
      val newLeft = insert(key, value, n.left)
      n.copy(left = Some(newLeft))
  }

  private def insertAlt(key: K, value: V, node: Node[K, V]): Node[K, V] = key match {
    case node.key => node.copy()
  }

  override def foreachDFS(f: (K, V) => Unit): Unit = null

  override def foreachBFS(f: (K, V) => Unit): Unit = null
}

object UnBalancedBinarySearchTree {
  def apply[K, V](key: K, value: V)(implicit ord: Ordering[K]): UnBalancedBinarySearchTree[K, V] = new UnBalancedBinarySearchTree(Node(key, value), ord)
}
