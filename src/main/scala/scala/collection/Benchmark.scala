package scala.collection

object Benchmark {
  type Job[A] = Seq[Int] => Set[A] => Unit
  type JavaJob[A] = Seq[Int] => java.util.TreeSet[A] => Unit
  type Builder[A] = () => Set[A]
  type JavaBuilder[A] = () => java.util.TreeSet[A]

  // builders
  val mutableSetBuilder: Builder[Int] = () => mutable.TreeSet[Int]()

  val immutableSetBuilder: Builder[Int] = () => immutable.TreeSet[Int]()

  val javaMutableSetBuilder: JavaBuilder[Int] = () => new java.util.TreeSet[Int]()

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

  val javaInsertionRemovalJob: JavaJob[Int] = (seq: Seq[Int]) => { (s: java.util.TreeSet[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d.add(i)
      }
      for (i <- seq) {
        d.remove(i)
      }
    }
  }

  val insertionRemovalJob: Job[Int] = (seq: Seq[Int]) => { (s: Set[Int]) =>
    {
      var d = s
      for (i <- seq) {
        d += i
      }
      for (i <- seq) {
        d -= i
      }
    }
  }

  def linearInsertionRemoval(n: Int): List[(Int, Long, Long, Long)] = {
    val init = (1 to n*step).toSeq
    (for (j <- 1 to n) yield (j * step)).map((a: Int) => {
      val seq = init.take(a)
      println(seq.size)
      (seq.size,
        measure(insertionRemovalJob)(seq)(immutableSetBuilder()),
        measure(insertionRemovalJob)(seq)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(seq)(javaMutableSetBuilder()))
    }).toList
  }

  val step = 10000

  def experimentInsertionRemoval(n: Int): List[(Int, Long, Long, Long)] = {
    val init = (1 to n*step).map(x => if (x %2 == 0) -x else x).toSeq.reverse
    (for (j <- 1 to n) yield (j * step)).map(a => {
      val seq = init.take(a)
      println(seq.size)
      (seq.size,
        measure(insertionRemovalJob)(seq)(immutableSetBuilder()),
        measure(insertionRemovalJob)(seq)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(seq)(javaMutableSetBuilder()))
    }).toList
  }

  def randomInsertionRemoval(n: Int): List[(Int, Long, Long, Long)] = {
    val init = (1 to n*step).toSeq.reverse
    (for (j <- 1 to n) yield (j * step)).map(a => {
      val seq = init.take(a)
      println(seq.size)
      (seq.size,
        measure(insertionRemovalJob)(seq)(immutableSetBuilder()),
        measure(insertionRemovalJob)(seq)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(seq)(javaMutableSetBuilder()))
    }).toList
  }

  def show(tab: Seq[(Int, Long, Long, Long)]) {
    println("#Cardinality scala.collection.immutable.TreeSet \"mutable TreeSet (based on an immutable AVL)\" java.util.TreeSet")
    tab.foreach(line => println("%s %s %s %s" format (line._1, line._2, line._3, line._4)))
  }

  def main(args: Array[String]) {
    show(experimentInsertionRemoval(args(0).toInt))
  }
}
