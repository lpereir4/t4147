/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import annotation.tailrec

sealed trait AVLTree[+A] {

  def balance: Int

  def depth: Int
}

case class Node[A](val data: A, val left: AVLTree[A], val right: AVLTree[A]) extends AVLTree[A] {

  override def balance: Int = right.depth - left.depth

  override def depth: Int = math.max(left.depth, right.depth) + 1

}

case object Leaf extends AVLTree[Nothing] {

  override def balance: Int = 0

  override def depth: Int = 0

}

object AVLTree {

  def insert[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): AVLTree[A] = {
    @tailrec
    def insertTC(value: A, tree: AVLTree[A], build: AVLTree[A] => AVLTree[A]): AVLTree[A] = tree match {
      case Leaf => build(Node(value, Leaf, Leaf))
      case Node(a, left, right) => if (-1 == ordering.compare(value, a))
        insertTC(value, left, x => build(rebalance(Node(a, x, right))))
      else
        insertTC(value, right, x => build(rebalance(Node(a, left, x))))
    }

    insertTC(value, tree, x => x)
  }

  def contains[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): Boolean = tree match {
    case Leaf => false
    case Node(a, left, right) => if (0 == ordering.compare(value, a))
      true
    else if (-1 == ordering.compare(value, a))
      contains(value, left, ordering)
    else
      contains(value, right, ordering)
  }

  def remove[A](value: A, tree: AVLTree[A], ordering: Ordering[A]): AVLTree[A] = tree match {
    // Empty
    case Leaf => throw new NoSuchElementException()

    // Leaf Leaf
    case Node(a, Leaf, Leaf) => if (0 == ordering.compare(value, a)) Leaf
    else throw new NoSuchElementException()

    // * Node
    case Node(a, left, right@Node(_, _, _)) => if (0 == ordering.compare(value, a)) {
      val (min, newRight) = removeMin(right)
      rebalance(Node(min, left, newRight))
    } else if (-1 == ordering.compare(value, a))
      rebalance(Node(a, remove(value, left, ordering), right))
    else
      rebalance(Node(a, left, remove(value, right, ordering)))

    // Node *
    case Node(a, left@Node(_, _, _), right) => if (0 == ordering.compare(value, a)) {
      val (max, newLeft) = removeMax(left)
      rebalance(Node(max, newLeft, right))
    } else if (-1 == ordering.compare(value, a))
      rebalance(Node(a, remove(value, left, ordering), right))
    else
      rebalance(Node(a, left, remove(value, right, ordering)))
  }

  def removeMax[A](tree: Node[A]): (A, AVLTree[A]) = {
    def removeMaxTC(tree: AVLTree[A], assemble: (A, AVLTree[A]) => (A, AVLTree[A])): (A, AVLTree[A]) = tree match {
      case Node(a, Leaf, Leaf) => assemble(a, Leaf)
      case Node(a, left, Leaf) => assemble(a, left)
      case Node(a, left, right) => removeMaxTC(right,
        (max: A, avl: AVLTree[A]) => assemble(max, rebalance(Node(a, left, avl))))
      case Leaf => sys.error("Should not append.")
    }
    removeMaxTC(tree, (a, b) => (a, b))
  }

  def removeMin[A](tree: Node[A]): (A, AVLTree[A]) = {
    def removeMinTC(tree: AVLTree[A], assemble: (A, AVLTree[A]) => (A, AVLTree[A])): (A, AVLTree[A]) = tree match {
      case Node(a, Leaf, Leaf) => assemble(a, Leaf)
      case Node(a, Leaf, right) => assemble(a, right)
      case Node(a, left, right) => removeMinTC(left,
        (min: A, avl: AVLTree[A]) => assemble(min, rebalance(Node(a, avl, right))))
      case Leaf => sys.error("Should not append.")
    }
    removeMinTC(tree, (a, b) => (a, b))
  }

  def toStream[A](tree: AVLTree[A], isLeftAcceptable: A => Boolean, isRightAcceptable: A => Boolean): Stream[A] = tree match {
    case Leaf => Stream.empty
    case Node(a, left, right) => if (isLeftAcceptable(a)) {
      if (isRightAcceptable(a))
        toStream(left, isLeftAcceptable, isRightAcceptable) ++ Stream(a) ++ toStream(right, isLeftAcceptable, isRightAcceptable)
      else
        toStream(left, isLeftAcceptable, isRightAcceptable)
    } else if (isRightAcceptable(a)) {
      toStream(right, isLeftAcceptable, isRightAcceptable)
    } else {
      Stream.empty
    }
  }

  def iterator[A](tree: AVLTree[A], isLeftAcceptable: A => Boolean, isRightAcceptable: A => Boolean): Iterator[A] =
    toStream(tree, isLeftAcceptable, isRightAcceptable).iterator

  def rebalance[A](tree: AVLTree[A]): AVLTree[A] = (tree, tree.balance) match {
    case (node@Node(_, left, _), -2) => left.balance match {
      case 1 => doubleRightRotation(node)
      case _ => rightRotation(node)
    }
    case (node@Node(_, _, right), 2) => right.balance match {
      case -1 => doubleLeftRotation(node)
      case _ => leftRotation(node)
    }
    case _ => tree
  }

  def leftRotation[A](tree: Node[A]): AVLTree[A] = tree.right match {
    case r@Node(b, left, right) => (1 == r.balance || 0 == r.balance) match {
      case true => Node(b, Node(tree.data, tree.left, left), right)
      case false => sys.error("Should not append.")
    }
    case Leaf => sys.error("Should not append.")
  }

  def rightRotation[A](tree: Node[A]): AVLTree[A] = tree.left match {
    case l@Node(b, left, right) => (-1 == l.balance || 0 == l.balance) match {
      case true => Node(b, left, Node(tree.data, right, tree.right))
      case false => sys.error("Should not append.")
    }
    case Leaf => sys.error("Should not append.")
  }

  def doubleLeftRotation[A](tree: Node[A]): AVLTree[A] = tree.right match {
    case right@Node(b, l, r) => leftRotation(Node(tree.data, tree.left, rightRotation(right)))
    case _ => sys.error("Should not append.")
  }

  def doubleRightRotation[A](tree: Node[A]): AVLTree[A] = tree.left match {
    case left@Node(b, l, r) => rightRotation(Node(tree.data, leftRotation(left), tree.right))
    case _ => sys.error("Should not append.")
  }

}
