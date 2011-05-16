package scala.collection

import mutable.TreeSet
import mutable.{ AVLTree, Node, Leaf }

object Main {
  def main(args: Array[String]) {
    for (j <- 1 to args(0).toInt) {
      mutableTreeSet(j * 10000)
      immutableTreeSet(j * 10000)
    }
  }

  def immutableTreeSet(i: Int) {
    implicit val o = Ordering[Int]
    val list = (1 to i)

    var d = System.nanoTime() / 1000000
    var b = immutable.TreeSet()
    for (i <- list) {
      b = b + i
    }
    for (i <- list) {
      b = b - i
    }
    var a = (System.nanoTime() / 1000000) - d
    println("@ " + i + " = " + a)
  }

  def mutableTreeSet(i: Int) {
    implicit val o = Ordering[Int]
    val list = (1 to i)

    var d = System.nanoTime() / 1000000
    val b = new TreeSet()
    for (i <- list) {
      b + i
    }
    for (i <- list) {
      b - i
    }
    var a = (System.nanoTime() / 1000000) - d
    println("> " + i + " = " + a)
  }
}
