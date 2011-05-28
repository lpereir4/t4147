package scala.collection

import mutable.{ AVLTree, Node, Leaf }

object Main {
  type Job[A] = Int => Set[A] => Unit
  type JavaJob[A] = Int => java.util.TreeSet[A] => Unit
  type Builder[A] = () => Set[A]
  type JavaBuilder[A] = () => java.util.TreeSet[A]

  // builders
  val mutableSetBuilder: Builder[Int] = () => mutable.TreeSet()

  val immutableSetBuilder: Builder[Int] = () => immutable.TreeSet()

  val javaMutableSetBuilder: JavaBuilder[Int] = () => new java.util.TreeSet[Int]()

  // chronometers
  def measure[A]: Job[A] => Int => Set[A] => Long = (job: Job[A]) => { (nb: Int) =>
    { (s: Set[A]) =>
      {
        val start = System.nanoTime()
        job(nb)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  def javaMeasure[A]: JavaJob[A] => Int => java.util.TreeSet[A] => Long = (job: JavaJob[A]) => { (nb: Int) =>
    { (s: java.util.TreeSet[A]) =>
      {
        val start = System.nanoTime()
        job(nb)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  val javaInsertionRemovalJob : JavaJob[Int] = (nb: Int) => { (s: java.util.TreeSet[Int]) =>
    {
      val list = (1 to nb)
      var d = s
      for (i <- list) {
        d.add(i)
      }
      for (i <- list) {
        d.remove(i)
      }
    }
  }

  val insertionRemovalJob: Job[Int] = (nb: Int) => { (s: Set[Int]) =>
    {
      val list = (1 to nb)
      var d = s
      for (i <- list) {
        d += i
      }
      for (i <- list) {
        d -= i
      }
    }
  }

  val sizeJob: Job[Int] = (nb: Int) => { (s: Set[Int]) =>
    {
      val a = s.size
    }
  }

  def insertionRemoval(n: Int): List[(Int, Long, Long, Long)] = {
    (for (j <- 1 to n) yield (j * 10000)).map(a =>
      (a,
        measure(insertionRemovalJob)(a)(immutableSetBuilder()),
        measure(insertionRemovalJob)(a)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(a)(javaMutableSetBuilder())
      )
    ).toList
  }

/*
  def size(n: Int): Seq[(Int, Long, Long)] = {
    var s = mutableSortedSetBuilder()
    val view = s.rangeImpl(None, None)
    (1 to n).map(i => {
      s += i
      (i, measure(sizeJob)(0)(s), measure(sizeJob)(0)(view))
    })
  }*/

  def show(tab: Seq[(Int, Long, Long, Long)]) {
    println("#Cardinality scala.collection.immutable.TreeSet \"mutable TreeSet (based on an immutable AVL)\" java.util.TreeSet")
    tab.foreach(line => println("%s %s %s %s" format(line._1, line._2, line._3, line._4)))
  }

  def main(args: Array[String]) {
    show(insertionRemoval(args(0).toInt))
    // show(size(args(0).toInt))
  }
}
