import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Gen
import org.scalacheck.{ Test => STest }
import org.scalacheck.ConsoleReporter

import collection.mutable.TreeSet

object MutableTreeSetSpecification extends Properties("Mutable TreeSet") {

  val generator = Gen.listOfN(1000, Gen.chooseNum(0, 1000))

  property("Insertion in TreeSet works properly.") = forAll(generator) {
    (s: List[Int]) => {
      implicit val o = Ordering[Int]
      val t = new TreeSet[Int]()
      for(a <- s) {
	t + a
      }
      t == s.toSet
    }
  }

  property("Removal from TreeSet works properly.") = forAll(generator) {
    (s: List[Int]) => {
      implicit val o = Ordering[Int]
      val t = new TreeSet[Int]()
      for(a <- s) {
	t + a
      }
      for(a <- s) {
	t - a
      }
      t.size == 0 && t == Set() 
    }
  }

  property("A set doesn't hold duplicates values.") = forAll(generator) {
    (s: List[Int]) => {
      implicit val o = Ordering[Int]
      val t = new TreeSet[Int]()
      for(a <- s) {
	t + a
      }
      t.size == s.distinct.size
    }
  }

  property("Elements are sorted.") = forAll(generator) {
    (s: List[Int]) => {
      implicit val o = Ordering[Int]
      val t = new TreeSet[Int]()
      for(a <- s) {
	t + a
      }
      t.toList == s.distinct.sorted
    }
  }
}
