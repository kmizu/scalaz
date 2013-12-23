package scalaz

import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import Cofree.CofreeZip
import Isomorphism._

object CofreeTest extends SpecLite {

  type CofreeLazyOption[A] = Cofree[LazyOption, A]
  type CofreeStream[A] = Cofree[Stream, A]
  type OneAndStream[A] = OneAnd[Stream, A]

  val oneAndStreamCofreeLazyOptionIso: OneAndStream <~> CofreeLazyOption =
    new IsoFunctorTemplate[OneAndStream, CofreeLazyOption] {
      def to[A](fa: OneAndStream[A]) =
        Cofree.unfold(fa){
          case OneAnd(a, h #:: t) => (a, LazyOption.lazySome(OneAnd(h, t)))
          case OneAnd(a, _)       => (a, LazyOption.lazyNone)
        }
      def from[A](fa: CofreeLazyOption[A]) =
        OneAnd(
          fa.head,
          fa.tail.map(s =>
            Foldable[CofreeLazyOption].foldRight(s, Stream.empty[A])(_ #:: _)
          ).getOrElse(Stream.empty)
        )
    }

  val treeCofreeStreamIso: Tree <~> CofreeStream =
    new IsoFunctorTemplate[Tree, CofreeStream] {
      def to[A](tree: Tree[A]): CofreeStream[A] =
        Cofree(tree.rootLabel, tree.subForest.map(to))
      def from[A](c: CofreeStream[A]): Tree[A] =
        Tree.node(c.head, c.tail.map(from(_)))
    }

  implicit def CofreeLazyOptionArb[A: Arbitrary]: Arbitrary[CofreeLazyOption[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[OneAndStream[A]]])(oneAndStreamCofreeLazyOptionIso.to(_))

  implicit def CofreeStreamArb[A: Arbitrary]: Arbitrary[CofreeStream[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[Tree[A]]])(treeCofreeStreamIso.to)

  checkAll("CofreeLazyOption", comonad.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", traverse1.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", monad.laws[CofreeLazyOption])
  checkAll("CofreeLazyOption", equal.laws[CofreeLazyOption[Int]])

  checkAll("CofreeStream", comonad.laws[CofreeStream])
  checkAll("CofreeStream", traverse1.laws[CofreeStream])
  checkAll("CofreeStream", monad.laws[CofreeStream])
  checkAll("CofreeStream", equal.laws[CofreeStream[Int]])

  {
    type CofreeZipLazyOption[A] = CofreeZip[LazyOption, A]

    implicit def CofreeZipLazyOptionArb[A: Arbitrary]: Arbitrary[CofreeZipLazyOption[A]] =
      Tags.Zip.subst(CofreeLazyOptionArb[A])

    // avoid stack overflow because `Applicative[CofreeZipLazyOption].point` is infinite stream
    implicit def CofreeZipLazyOptionEqual[A: Equal]: Equal[CofreeZipLazyOption[A]] =
      Equal.equalBy{ a =>
        val OneAnd(h, t) = oneAndStreamCofreeLazyOptionIso.from(a)
        h -> t.take(1000)
      }

    checkAll("CofreeZipLazyOption", applicative.laws[CofreeZipLazyOption])
  }

  {
    type CofreeZipStream[A] = CofreeZip[Stream, A]

    implicit def CofreeZipStreamArb[A: Arbitrary]: Arbitrary[CofreeZipStream[A]] =
      Tags.Zip.subst(CofreeStreamArb[A])

    checkAll("CofreeZipStream", ScalazProperties.apply.laws[CofreeZipStream])
  }

  "Applicative[({type λ[α] = CofreeZip[LazyOption, α]})#λ] is Applicative[({type λ[α]=Stream[α] @@ Zip})#λ]" ! forAll{
    (a: OneAndStream[Int], b: OneAndStream[Int]) =>

    import syntax.foldable._
    val f = (_: Int) + (_: Int)
    val h #:: t = Applicative[({type λ[α]=Stream[α] @@ Tags.Zip})#λ].apply2(Tags.Zip(a.toStream), Tags.Zip(b.toStream))(f)

    val aa = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(a))
    val bb = Tags.Zip(oneAndStreamCofreeLazyOptionIso.to(b))
    val y = Applicative[({type λ[α] = CofreeZip[LazyOption, α]})#λ].apply2(aa, bb)(f)
    OneAnd(h, t) must_=== oneAndStreamCofreeLazyOptionIso.from(y)
  }

  object instances{
    def comonad[F[_]] = Comonad[({type λ[α] = Cofree[F, α]})#λ]
    def bind[F[_]: Plus: Functor] = Bind[({type λ[α] = Cofree[F, α]})#λ]
    def monad[F[_]: PlusEmpty: Functor] = Monad[({type λ[α] = Cofree[F, α]})#λ]
    def foldable1[F[_]: Foldable] = Foldable1[({type λ[α] = Cofree[F, α]})#λ]
    def traverse1[F[_]: Traverse] = Traverse1[({type λ[α] = Cofree[F, α]})#λ]

    // checking absence of ambiguity
    def bind[F[_]: PlusEmpty: Functor] = Bind[({type λ[α] = Cofree[F, α]})#λ]
    def functor[F[_]: PlusEmpty: Traverse] = Functor[({type λ[α] = Cofree[F, α]})#λ]
    def foldable1[F[_]: Traverse1] = Foldable1[({type λ[α] = Cofree[F, α]})#λ]
    def traverse1[F[_]: Traverse1] = Traverse1[({type λ[α] = Cofree[F, α]})#λ]

    object zip{
      def functor[F[_]] = Functor[({type λ[α] = CofreeZip[F, α]})#λ]
      def apply[F[_]: Apply] = Apply[({type λ[α] = CofreeZip[F, α]})#λ]
      def applicative[F[_]: Applicative] = Applicative[({type λ[α] = CofreeZip[F, α]})#λ]

      // checking absence of ambiguity
      def functor[F[_]: Applicative] = Functor[({type λ[α] = CofreeZip[F, α]})#λ]
      def apply[F[_]: Applicative] = Apply[({type λ[α] = CofreeZip[F, α]})#λ]
    }

  }
}

