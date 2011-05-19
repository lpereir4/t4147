package scala.collection

import mutable.TreeSet
import mutable.{ AVLTree, Node, Leaf }

object Main {
  type Job[A] = Int => Builder[A] => Unit
  type Builder[A] = () => Set[A]
  
  // builders
  val mutableSetBuilder: Builder[Int] = () => new TreeSet[Int]()
  
  val immutableSetBuilder: Builder[Int] = () => immutable.TreeSet()

  // chronometers
  def measure[A]: Job[A] => Int => Builder[A] => Long = (job: Job[A]) => {
    (nb: Int) => {
	  (b: Builder[A]) => {
	    val start = System.nanoTime()
		job(nb)(b)
		(System.nanoTime() - start) / 1000000
	  }
	}
  }

  // job1
  val job1: Job[Int] = (nb: Int) => {
    (b: Builder[Int]) => {
	  val list = (1 to nb)
	  var d = b()
	  for (i <- list) {
	    d = d + i
	  }
	  for (i <- list) {
	    d = d - i
	  }
	}
  }

  def main(args: Array[String]) {
    val m: Seq[(Int, Long, Long)] = (for (j <- 1 to args(0).toInt) yield(j*10000)).map(a => (a, measure(job1)(a)(mutableSetBuilder), measure(job1)(a)(immutableSetBuilder))).toList
	println(m.map(_._1).mkString(";"))
	println(m.map(_._2).mkString(";"))
	println(m.map(_._3).mkString(";"))
  }
}
