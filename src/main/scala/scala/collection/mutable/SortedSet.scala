/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package mutable

import generic._

/**
 * 
 *  @author Lucien Pereira
 *  @define Coll mutable.SortedSet
 *  @define coll mutable sorted set
 * 
 */
trait SortedSet[A] extends collection.SortedSet[A] with collection.SortedSetLike[A,SortedSet[A]] {
  /** Needs to be overridden in subclasses. */
  override def empty: SortedSet[A] = SortedSet.empty[A]
}

/** $factoryInfo
 *  @define Coll mutable.SortedSet
 *  @define coll mutable sorted set
 */
object SortedSet extends MutableSortedSetFactory[SortedSet] {
  /** $sortedSetCanBuildFromInfo */
  implicit def canBuildFrom[A](implicit ord: Ordering[A]): CanBuildFrom[Coll, A, SortedSet[A]] = new SortedSetCanBuildFrom[A]
  def empty[A](implicit ord: Ordering[A]): SortedSet[A] = TreeSet.empty[A]
}
