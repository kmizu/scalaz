package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scala.util.Random
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object DievTest extends SpecLite {
  val random = new Random()

  "toEStream" ! forAll {
    list: List[Int] =>
    val diev = Diev.fromValuesSeq(list)
    diev.toEStream.toList must_=== diev.toList
  }

  "length" ! forAll {
    set: Set[Int] =>
    val list = set.toList.sorted
    val diev = Diev.fromValuesSeq(list)
    diev.length must_=== list.length
  }

  "foldRight" ! forAll {
    set: Set[Int] =>
    val list = set.toList.sorted
    val diev = Diev.fromValuesSeq(list)
    diev.foldRight(List[Int]())(_ :: _) must_=== list.foldRight(List[Int]())(_ :: _)
  }

  "insert order makes no difference" ! forAll {
    (list: List[Int]) => {
      val shuffledList = random.shuffle(list)
      val dievFromList = list.foldLeft(Diev.empty[Int])(_ + _)
      val dievFromShuffledList = shuffledList.foldLeft(Diev.empty[Int])(_ + _)
      dievFromList must_===(dievFromShuffledList)
    }
  }

  "fixIntervalOrder" ! forAll {
    (tuple: (Int, Int)) => {
      val expectedResult = if (tuple._1 > tuple._2) tuple.swap else tuple
      DievInterval.fixIntervalOrder(tuple) must_===(expectedResult)
    }
  }

  // TODO: Use data table to test subtractInterval.

  "fromValuesSeq / toSet" ! forAll {
    (set: Set[Int]) => Diev.fromValuesSeq(set.toSeq).toSet must_===(set)
  }

  "fromValuesSeq / toList" ! forAll {
    (list: List[Int]) => {
      val sortedList = list.toSet.toList.sorted
      Diev.fromValuesSeq(list).toList must_===(sortedList)
    }
  }

  "++ associativity" ! forAll {
    (first: Diev[Int], second: Diev[Int]) => first ++ second must_===(second ++ first)
  }

  "intervals / fromIntervalsSeq" ! forAll {
    (original: Diev[Int]) => Diev.fromIntervalsSeq(original.intervals) must_===(original)
  }

  "-- / ++" ! forAll {
    (first: Diev[Int], second: Diev[Int]) => first -- second ++ second must_===(first ++ second)
  }

  checkAll(equal.laws[Diev[Int]])
  checkAll(monoid.laws[Diev[Int]])

  {
    import org.scalacheck._
    val listArb = Arbitrary(Gen.listOf(Gen.choose(-1000, 1000)))
    implicit val dievArb = dievArbitrary[Int](listArb, implicitly)
    checkAll(foldable.laws[Diev])
  }
}
