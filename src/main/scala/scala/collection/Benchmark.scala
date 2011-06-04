package scala.collection

object Benchmark {
  type Job[A] = Seq[Int] => Set[A] => Unit
  type JavaJob[A] = Seq[Int] => java.util.TreeSet[A] => Unit
  type Builder[A] = Seq[A] => Set[A]
  type JavaBuilder[A] = Seq[A] => java.util.TreeSet[A]

  // builders
  val mutableSetBuilder: Builder[Int] = (seq: Seq[Int]) => mutable.TreeSet[Int](seq: _*)

  val immutableSetBuilder: Builder[Int] = (seq: Seq[Int]) => immutable.TreeSet[Int](seq: _*)

  val javaMutableSetBuilder: JavaBuilder[Int] = (seq: Seq[Int]) => {
    val a = new java.util.TreeSet[Int]()
    for(i <- seq)
      a.add(i)
    a
  }

  // chronometers
  def measure[A]: Job[A] => Seq[Int] => Set[A] => Long = (job: Job[A]) => { (seq: Seq[Int]) =>
    { (s: Set[A]) =>
      {
        val start = System.nanoTime()
        job(seq)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  def javaMeasure[A]: JavaJob[A] => Seq[Int] => java.util.TreeSet[A] => Long = (job: JavaJob[A]) => { (seq: Seq[Int]) =>
    { (s: java.util.TreeSet[A]) =>
      {
        val start = System.nanoTime()
        job(seq)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  val javaInsertionJob: JavaJob[Int] = (seq: Seq[Int]) => { (s: java.util.TreeSet[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d.add(i)
      }
    }
  }

  val javaRemovalJob: JavaJob[Int] = (seq: Seq[Int]) => { (s: java.util.TreeSet[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d.remove(i)
      }
    }
  }

  val insertionJob: Job[Int] = (seq: Seq[Int]) => { (s: Set[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d += i
      }
    }
  }

  val removalJob: Job[Int] = (seq: Seq[Int]) => { (s: Set[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d -= i
      }
    }
  }

  def regularData(size: Int): (String, Seq[Int]) = ("regularData", (1 to size))

  def reverseData(size: Int): (String, Seq[Int]) = ("reverseData", regularData(size)._2.reverse)

  def alternateData(size: Int): (String, Seq[Int]) = ("alternateData", regularData(size)
						      ._2
						      .map(x => if (x %2 == 0) -x else x).reverse)

  def genericRemoval(n: Int, step: Int, f: (Int) => (String, Seq[Int])): (String, List[(Int, Long, Long, Long)]) = {
    val data = f(step*n)
    ("REMOVAL : "+data._1, (for (j <- 1 to n) yield (j * step)).map((a: Int) => {
      val seq = data._2.take(a)
      (seq.size,
        measure(insertionJob)(seq)(immutableSetBuilder(seq)),
        measure(insertionJob)(seq)(mutableSetBuilder(seq)),
        javaMeasure(javaInsertionJob)(seq)(javaMutableSetBuilder(seq)))
    }).toList)
  }

  def genericInsertion(n: Int, step: Int, f: (Int) => (String, Seq[Int])): (String, List[(Int, Long, Long, Long)]) = {
    val data = f(step*n)
    ("INSERTION : "+data._1, (for (j <- 1 to n) yield (j * step)).map((a: Int) => {
      val seq = data._2.take(a)
      (seq.size,
        measure(insertionJob)(seq)(immutableSetBuilder(Nil)),
        measure(insertionJob)(seq)(mutableSetBuilder(Nil)),
        javaMeasure(javaInsertionJob)(seq)(javaMutableSetBuilder(Nil)))
    }).toList)
  }

  def show(tab: (String, Seq[(Int, Long, Long, Long)])) {
    println(tab._1)
    println("#Cardinality scala.collection.immutable.TreeSet \"mutable TreeSet (based on an immutable AVL)\" java.util.TreeSet")
    tab._2.foreach(line => println("%s %s %s %s" format (line._1, line._2, line._3, line._4)))
  }

  def main(args: Array[String]) {
    val step = 10000
    show(genericInsertion(args(0).toInt, step , regularData))
    println("---------------")
    show(genericRemoval(args(0).toInt, step , regularData))
  }
}
