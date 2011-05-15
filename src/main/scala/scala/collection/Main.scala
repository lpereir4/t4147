package scala.collection

import mutable.TreeSet
import mutable.{AVLTree, Node, Leaf}

object Main {

  def main(args: Array[String]) {
    implicit val o = Ordering[Int]

    val b = new TreeSet()
    val view = b.rangeImpl(Some(3), Some(7))
    for(i <- (1 to args(0).toInt).reverse) {
      b + i
    }
    println(b)
    println(view)
    for(a <- 1 to args(0).toInt) {
      if(0 == a % args(1).toInt)
	b - a
    }
    println("===")
    println(b)
    println(view)
  }
}
