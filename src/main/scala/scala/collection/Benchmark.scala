package scala.collection

object Benchmark {
  type Job[A] = Seq[Long] => Set[A] => Unit
  type JavaJob[A] = Seq[Long] => java.util.TreeSet[A] => Unit
  type Builder[A] = () => Set[A]
  type JavaBuilder[A] = () => java.util.TreeSet[A]

  // builders
  val mutableSetBuilder: Builder[Long] = () => mutable.TreeSet()

  val immutableSetBuilder: Builder[Long] = () => immutable.TreeSet()

  val javaMutableSetBuilder: JavaBuilder[Long] = () => new java.util.TreeSet[Long]()

  // chronometers
  def measure[A]: Job[A] => Seq[Long] => Set[A] => Long = (job: Job[A]) => { (seq: Seq[Long]) =>
    { (s: Set[A]) =>
      {
        val start = System.nanoTime()
        job(seq)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  def javaMeasure[A]: JavaJob[A] => Seq[Long] => java.util.TreeSet[A] => Long = (job: JavaJob[A]) => { (seq: Seq[Long]) =>
    { (s: java.util.TreeSet[A]) =>
      {
        val start = System.nanoTime()
        job(seq)(s)
        (System.nanoTime() - start) / 1000000
      }
    }
  }

  val javaInsertionRemovalJob: JavaJob[Long] = (seq: Seq[Long]) => { (s: java.util.TreeSet[Long]) =>
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

  val insertionRemovalJob: Job[Long] = (seq: Seq[Long]) => { (s: Set[Long]) =>
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

  def fibo(n: Long): Long = {
    def fiboTC(n: Long, accu: (Long, Long)): Long = n match {
      case 0 => accu._2
      case 1 => accu._1
      case a => fiboTC(a-1, (accu._1+accu._2, accu._1))
    }
    fiboTC(math.max(0, n), (1, 0))
  }

  def linearInsertionRemoval(n: Long): List[(Long, Long, Long, Long)] = {
    (for (j <- 1L to n) yield (j * 10000)).map((a:Long) => {
      val seq: List[Long] = List(1L to a: _*)//.map(x => math.abs(fibo(x))).distinct
      (a,
        measure(insertionRemovalJob)(seq)(immutableSetBuilder()),
        measure(insertionRemovalJob)(seq)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(seq)(javaMutableSetBuilder())
      )
    }
    ).toList
  }

  def fiboInsertionRemoval(n: Long): List[(Long, Long, Long, Long)] = {
    (for (j <- 1L to n) yield (j * 10000)).map((a:Long) => {
      val seq  = (1 to a.toInt).map(x => math.abs(fibo(x))).distinct
      (a,
        measure(insertionRemovalJob)(seq)(immutableSetBuilder()),
        measure(insertionRemovalJob)(seq)(mutableSetBuilder()),
        javaMeasure(javaInsertionRemovalJob)(seq)(javaMutableSetBuilder())
      )
    }
    ).toList
  }

  def show(tab: Seq[(Long, Long, Long, Long)]) {
    println("#Cardinality scala.collection.immutable.TreeSet \"mutable TreeSet (based on an immutable AVL)\" java.util.TreeSet")
    tab.foreach(line => println("%s %s %s %s" format (line._1, line._2, line._3, line._4)))
  }

  def main(args: Array[String]) {
    show(linearInsertionRemoval(args(0).toInt))
  }
}
