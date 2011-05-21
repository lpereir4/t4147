package scala.collection

import mutable.{ AVLTree, Node, Leaf }

object Main {
  type Job[A] = Int => SortedSet[A] => Unit
  type Builder[A] = () => SortedSet[A]

  // builders
  val mutableSetBuilder: Builder[Int] = () => mutable.TreeSet()

  val immutableSetBuilder: Builder[Int] = () => immutable.TreeSet()

  // chronometers
  def measure[A]: Job[A] => Int => SortedSet[A] => Long = (job: Job[A]) => { (nb: Int) =>
    { (s: SortedSet[A]) =>
      {
        val start = System.nanoTime()
        job(nb)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  val insertionRemovalJob: Job[Int] = (nb: Int) => { (s: SortedSet[Int]) =>
    {
      val list = (1 to nb)
      var d = s
      for (i <- list) {
        d = d + i
      }
      for (i <- list) {
        d = d - i
      }
    }
  }

  val sizeJob: Job[Int] = (nb: Int) => { (s: SortedSet[Int]) =>
    {
      val a = s.size
    }
  }

  def insertionRemoval(n: Int): Seq[(Int, Long, Long)] = {
    (for (j <- 1 to n) yield (j * 10000)).map(a =>
      (a,
        measure(insertionRemovalJob)(a)(mutableSetBuilder()),
        measure(insertionRemovalJob)(a)(immutableSetBuilder()))).toList
  }

  def size(n: Int): Seq[(Int, Long, Long)] = {
    val s = mutableSetBuilder()
    val view = s.rangeImpl(None, None)
    (1 to n).map(i => {
      s + i
      (i, measure(sizeJob)(0)(s), measure(sizeJob)(0)(view))
    })
  }

  def show(tab: Seq[(Int, Long, Long)]) {
    println(tab.map(_._1).mkString(";"))
    println(tab.map(_._2).mkString(";"))
    println(tab.map(_._3).mkString(";"))
  }

  def main(args: Array[String]) {
    // show(insertionRemoval(args(0).toInt))
    // show(size(args(0).toInt))
  }
}
