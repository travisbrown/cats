package cats

import cats.kernel._
import simulacrum.typeclass
import cats.kernel.compat.scalaVersionSpecific._
import scala.collection.immutable.SortedMap
import scala.util.Try

/**
 * Must obey the laws defined in cats.laws.InvariantLaws.
 */
@typeclass trait Invariant[F[_]] { self =>

  /**
   * Transform an `F[A]` into an `F[B]` by providing a transformation from `A`
   * to `B` and one from `B` to `A`.
   *
   * Example:
   * {{{
   * scala> import cats.implicits._
   * scala> import scala.concurrent.duration._
   * scala> val durSemigroup: Semigroup[FiniteDuration] =
   *      | Invariant[Semigroup].imap(Semigroup[Long])(Duration.fromNanos)(_.toNanos)
   * scala> durSemigroup.combine(2.seconds, 3.seconds)
   * res1: FiniteDuration = 5 seconds
   * }}}
   */
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]

  def compose[G[_]: Invariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariant[F, G] {
      val F = self
      val G = Invariant[G]
    }

  def composeFunctor[G[_]: Functor]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantCovariant[F, G] {
      val F = self
      val G = Functor[G]
    }

  def composeContravariant[G[_]: Contravariant]: Invariant[λ[α => F[G[α]]]] =
    new ComposedInvariantContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
object Invariant extends DistributiveInstances {
  implicit def catsInstancesForId: Distributive[Id] with Comonad[Id] = cats.catsInstancesForId
  implicit def catsComonadForTuple2[A]: Comonad[(A, *)] = cats.instances.tuple.catsStdInstancesForTuple2[A]
  implicit def catsMonadErrorForEither[A]: MonadError[Either[A, *], A] =
    cats.instances.either.catsStdInstancesForEither[A]
  implicit def catsInstancesForOption: MonadError[Option, Unit] with Alternative[Option] =
    cats.instances.option.catsStdInstancesForOption
  implicit def catsInstancesForList: Monad[List] with Alternative[List] with CoflatMap[List] =
    cats.instances.list.catsStdInstancesForList
  implicit def catsInstancesForVector: Monad[Vector] with Alternative[Vector] with CoflatMap[Vector] =
    cats.instances.vector.catsStdInstancesForVector
  implicit def catsInstancesForStream: Monad[Stream] with Alternative[Stream] with CoflatMap[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  implicit def catsFlatMapForSortedMap[K: Order]: FlatMap[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]
  implicit def catsBimonadForFunction0[I]: Bimonad[Function0] = cats.instances.function.catsStdBimonadForFunction0
  implicit def catsMonadForFunction1[I]: Monad[I => *] = cats.instances.function.catsStdMonadForFunction1[I]
  implicit val catsFunctorForPair: Functor[λ[P => (P, P)]] = cats.instances.tuple.catsDataFunctorForPair

  implicit def catsInstancesForTry: MonadError[Try, Throwable] with CoflatMap[Try] with Traverse[Try] with Monad[Try] =
    cats.instances.try_.catsStdInstancesForTry

  implicit val catsContravariantMonoidalForOrder: ContravariantMonoidal[Order] =
    cats.instances.order.catsContravariantMonoidalForOrder
  implicit val catsContravariantMonoidalForPartialOrder: ContravariantMonoidal[PartialOrder] =
    cats.instances.partialOrder.catsContravariantMonoidalForPartialOrder
  implicit val catsInvariantMonoidalForSemigroup: InvariantMonoidal[Semigroup] =
    cats.instances.invariant.catsInvariantMonoidalSemigroup

  implicit val catsInvariantMonoid: Invariant[Monoid] = new Invariant[Monoid] {

    def imap[A, B](fa: Monoid[A])(f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }

  implicit val catsInvariantBand: Invariant[Band] = new Invariant[Band] {

    def imap[A, B](fa: Band[A])(f: A => B)(g: B => A): Band[B] = new Band[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }
  }

  implicit val catsInvariantSemilattice: Invariant[Semilattice] = new Invariant[Semilattice] {

    def imap[A, B](fa: Semilattice[A])(f: A => B)(g: B => A): Semilattice[B] = new Semilattice[B] {
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }

  implicit val catsInvariantCommutativeMonoid: Invariant[CommutativeMonoid] = new Invariant[CommutativeMonoid] {

    def imap[A, B](fa: CommutativeMonoid[A])(f: A => B)(g: B => A): CommutativeMonoid[B] = new CommutativeMonoid[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }

  implicit val catsInvariantBoundedSemilattice: Invariant[BoundedSemilattice] = new Invariant[BoundedSemilattice] {

    def imap[A, B](fa: BoundedSemilattice[A])(f: A => B)(g: B => A): BoundedSemilattice[B] = new BoundedSemilattice[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }

  implicit val catsInvariantGroup: Invariant[Group] = new Invariant[Group] {

    def imap[A, B](fa: Group[A])(f: A => B)(g: B => A): Group[B] = new Group[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }

  implicit val catsInvariantCommutativeGroup: Invariant[CommutativeGroup] = new Invariant[CommutativeGroup] {

    def imap[A, B](fa: CommutativeGroup[A])(f: A => B)(g: B => A): CommutativeGroup[B] = new CommutativeGroup[B] {
      val empty = f(fa.empty)
      def combine(x: B, y: B): B = f(fa.combine(g(x), g(y)))
      def inverse(b: B): B = f(fa.inverse(g(b)))
      override def combineAllOption(bs: IterableOnce[B]): Option[B] =
        fa.combineAllOption(bs.iterator.map(g)).map(f)
    }

  }
}

private[cats] trait DistributiveInstances {
  implicit def catsDistributiveForFunction0: Distributive[Function0] = cats.instances.function.function0Distributive
  implicit def catsDistributiveForFunction1[I]: Distributive[I => *] =
    cats.instances.function.catsStdDistributiveForFunction1[I]
}
