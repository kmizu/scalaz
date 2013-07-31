package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import ImmutableArray.WrappedImmutableArray

class ImmutableArrayTest extends Spec {

  checkAll(monoid.laws[WrappedImmutableArray[Int]])
  checkAll(order.laws[WrappedImmutableArray[Int]])

}
