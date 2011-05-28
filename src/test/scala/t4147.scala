import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Gen
import org.scalacheck.{ Test => STest }
import org.scalacheck.ConsoleReporter

import collection.mutable.TreeSet

object MutableTreeSetSpecification extends Properties("Mutable TreeSet") {

  val generator = Gen.listOfN(1000, Gen.chooseNum(0, 1000))

  property("Insertion in TreeSet works properly.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s: _*)
      t == s.toSet
    }
  }

  property("Removal from TreeSet works properly.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s: _*)
      for (a <- s) {
        t -= a
      }
      t.size == 0 && t == Set()
    }
  }

  property("A set doesn't hold duplicates values.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s: _*)
      t.size == s.distinct.size
    }
  }

  property("Elements are sorted.") = forAll(generator) { (s: List[Int]) =>
    {
      val t = TreeSet[Int](s: _*)
      t.toList == s.distinct.sorted
    }
  }

  property("implicit can build from resolution succeeds as well as the \"same-result-type\" principle.") =
    forAll(generator) { (s: List[Int]) =>
      {
        val t = TreeSet[Int](s: _*)
        val t2 = t.map(_*2)
	t2.isInstanceOf[collection.mutable.TreeSet[Int]]
      }
    }
}
