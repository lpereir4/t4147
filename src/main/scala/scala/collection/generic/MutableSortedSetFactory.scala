/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

import scala.collection.mutable.{Builder, SetBuilder}

/**
 *  
 *  @define Coll mutable.SortedSet
 *  @define coll mutable sorted 
 *  @author Lucien Pereira
 * 
 */
abstract class MutableSortedSetFactory[CC[A] <: mutable.SortedSet[A] with SortedSetLike[A, CC[A]] with mutable.Set[A] with mutable.SetLike[A, CC[A]]] extends SortedSetFactory[CC]
