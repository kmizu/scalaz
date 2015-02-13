package scalaz

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import Tags._

object MaybeTest extends SpecLite {
  import org.scalacheck.Arbitrary
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import std.anyVal._
  import std.string._

  import Maybe._

  checkAll("Maybe", order.laws[Maybe[Int]])
  checkAll("Maybe @@ First", order.laws[Maybe[Int] @@ First])
  checkAll("Maybe @@ Last", order.laws[Maybe[Int] @@ Last])
  checkAll("Maybe @@ Min", order.laws[MinMaybe[Int]])
  checkAll("Maybe @@ Max", order.laws[MaxMaybe[Int]])

  checkAll("Maybe", monoid.laws[Maybe[Int]])
  checkAll("Maybe @@ First", monoid.laws[Maybe[Int] @@ First])
  checkAll("Maybe @@ Last", monoid.laws[Maybe[Int] @@ Last])
  checkAll("Maybe @@ Min", monoid.laws[MinMaybe[Int]])
  checkAll("Maybe @@ Max", monoid.laws[MaxMaybe[Int]])

  checkAll("Maybe @@ First", monad.laws[FirstMaybe])
  checkAll("Maybe @@ Last", monad.laws[LastMaybe])
  checkAll("Maybe @@ Min", monad.laws[MinMaybe])
  checkAll("Maybe @@ Max", monad.laws[MaxMaybe])

  checkAll(monadPlus.strongLaws[Maybe])
  checkAll(traverse.laws[Maybe])
  checkAll(isEmpty.laws[Maybe])
  checkAll(equal.laws[Maybe[Int]])

  "Empty is less than anything else" ! forAll { x: Maybe[Int] => Order[Maybe[Int]].greaterThanOrEqual(x, Maybe.empty) }

  "Empty is ignored in Maybe[A]@@Min" ! forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Min(x) |+| Min(empty)) must_=== Min(x)
  }

  "Empty is ignored in Maybe[A]@@Max" ! forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Max(x) |+| Max(empty)) must_=== Max(x)
  }

  "just toFailure is failure" ! forAll { (x: Int, s: String) => just(x).toFailure(s).isFailure }

  "empty toFailure is success" ! forAll { s: String => empty.toFailure(s).isSuccess }

  "just toSuccess is success" ! forAll { (x: Int, s: String) => just(x).toSuccess(s).isSuccess }

  "empty toSuccess is failure" ! forAll { s: String => empty.toSuccess(s).isFailure }

  "just toLeft is left" ! forAll { (x: Int, s: String) => just(x).toLeft(s).isLeft }

  "empty toLeft is right" ! forAll { s: String => empty.toLeft(s).isRight }

  "just toRight is right" ! forAll { (x: Int, s: String) => just(x).toRight(s).isRight }

  "empty toRight is left" ! forAll { s: String => empty.toRight(s).isLeft }

  "just isJust" ! forAll { x: Int => just(x).isJust }

  "just isn't empty" ! forAll { x: Int => !just(x).isEmpty }

  "empty is empty" ! check(empty.isEmpty)

  "empty isn't just" ! check(!empty.isJust)

  "just to option is some" ! forAll { x: Int => just(x).toOption.isDefined }

  "empty to option is none" ! check(empty.toOption.isEmpty)

  "just orElse is just" ! forAll { (x: Int, m: Maybe[Int]) => just(x).orElse(m).isJust }

  "fromNullable(null) is Empty" ! check {
    val s: String = null
    Maybe.fromNullable(s).isEmpty
  }

  "fromNullable(notNull) is just" ! forAll { (s: String) => Maybe.fromNullable(s) must_=== just(s) }

  object instances {
    def equal[A: Equal] = Equal[Maybe[A]]
    def order[A: Order] = Order[Maybe[A]]
    def semigroup[A: Semigroup] = Monoid[Maybe[A]]
    def monad[A] = Monad[Maybe]

    def monoidFirst[A] = Monoid[Maybe[A] @@ First]
    def monoidLast[A] = Monoid[Maybe[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Maybe[A]]
  }
}
