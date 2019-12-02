package cats
package data

/** Similar to [[cats.data.Tuple2K]], but for nested composition.
 *
 * For instance, since both `List` and `Option` have a `Functor`, then so does
 * `List[Option[_]]`. This is represented by this data type via the instantiation
 * `Nested[List, Option, *]`.
 *
 * {{{
 * scala> import cats.Functor
 * scala> import cats.data.Nested
 * scala> import cats.implicits._
 * scala> val listOption: List[Option[Int]] = List(Some(1), None)
 * scala> val f: Int => String = i => (i * 2).toString
 * scala> Functor[List].map(listOption)(opt => opt.map(f))
 * res0: List[Option[String]] = List(Some(2), None)
 * scala> val nested: Nested[List, Option, Int] = Nested(listOption)
 * scala> val result: Nested[List, Option, String] = Functor[Nested[List, Option, *]].map(nested)(f)
 * scala> result.value
 * res1: List[Option[String]] = List(Some(2), None)
 * }}}
 */
final case class Nested[F[_], G[_], A](value: F[G[A]]) {

  /**
   * Modify the context `F` using transformation `f`.
   */
  def mapK[H[_]](f: F ~> H): Nested[H, G, A] =
    Nested(f(value))

}

object Nested extends NestedInstances

sealed abstract private[data] class NestedInstances extends NestedInstances0 {
  implicit def catsDataEqForNested[F[_], G[_], A](implicit FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] =
    Eq.by[Nested[F, G, A], F[G[A]]](_.value)

  implicit def catsDataNonEmptyTraverseForNested[F[_]: NonEmptyTraverse, G[_]: NonEmptyTraverse]
    : NonEmptyTraverse[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedNonEmptyTraverse[F, G] {
      val FG: NonEmptyTraverse[({ type λ[α] = F[G[α]] })#λ] = NonEmptyTraverse[F].compose[G]
    }

  implicit def catsDataContravariantMonoidalForApplicativeForNested[F[_]: Applicative, G[_]: ContravariantMonoidal]
    : ContravariantMonoidal[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedContravariantMonoidal[F, G] with NestedContravariant[F, G] {
      val FG: ContravariantMonoidal[({ type λ[α] = F[G[α]] })#λ] = Applicative[F].composeContravariantMonoidal[G]
    }

  implicit def catsDataDeferForNested[F[_], G[_]](implicit F: Defer[F]): Defer[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new Defer[({ type λ[α$] = Nested[F, G, α$] })#λ] {
      def defer[A](fa: => Nested[F, G, A]): Nested[F, G, A] =
        Nested(F.defer(fa.value))
    }

  implicit def catsDataTraverseFilterForNested[F[_], G[_]](
    implicit F0: Traverse[F],
    G0: TraverseFilter[G]
  ): TraverseFilter[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedTraverseFilter[F, G] {
      implicit val F: Traverse[F] = F0
      implicit val G: TraverseFilter[G] = G0
    }
}

sealed abstract private[data] class NestedInstances0 extends NestedInstances1 {
  implicit def catsDataTraverseForNested[F[_]: Traverse, G[_]: Traverse]
    : Traverse[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedTraverse[F, G] {
      val FG: Traverse[({ type λ[α] = F[G[α]] })#λ] = Traverse[F].compose[G]
    }

  implicit def catsDataFunctorFilterForNested[F[_], G[_]](
    implicit F0: Functor[F],
    G0: FunctorFilter[G]
  ): FunctorFilter[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedFunctorFilter[F, G] {
      implicit val F: Functor[F] = F0
      implicit val G: FunctorFilter[G] = G0
    }
}

sealed abstract private[data] class NestedInstances1 extends NestedInstances2 {
  implicit def catsDataReducibleForNested[F[_]: Reducible, G[_]: Reducible]
    : Reducible[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedReducible[F, G] {
      val FG: Reducible[({ type λ[α] = F[G[α]] })#λ] = Reducible[F].compose[G]
    }

  implicit def catsDataFunctorForContravariantForNested[F[_]: Contravariant, G[_]: Contravariant]
    : Functor[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedFunctor[F, G] {
      val FG: Functor[({ type λ[α] = F[G[α]] })#λ] = Contravariant[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances2 extends NestedInstances3 {
  implicit def catsDataFoldableForNested[F[_]: Foldable, G[_]: Foldable]
    : Foldable[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedFoldable[F, G] {
      val FG: Foldable[({ type λ[α] = F[G[α]] })#λ] = Foldable[F].compose[G]
    }

  implicit def catsDataContravariantForCovariantNested[F[_]: Contravariant, G[_]: Functor]
    : Contravariant[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[({ type λ[α] = F[G[α]] })#λ] = Contravariant[F].composeFunctor[G]
    }
}

sealed abstract private[data] class NestedInstances3 extends NestedInstances4 {
  implicit def catsDataAlternativeForNested[F[_]: Alternative, G[_]: Applicative]
    : Alternative[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedAlternative[F, G] {
      val FG: Alternative[({ type λ[α] = F[G[α]] })#λ] = Alternative[F].compose[G]
    }

  implicit def catsDataContravariantForContravariantNested[F[_]: Functor, G[_]: Contravariant]
    : Contravariant[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedContravariant[F, G] {
      val FG: Contravariant[({ type λ[α] = F[G[α]] })#λ] = Functor[F].composeContravariant[G]
    }
}

sealed abstract private[data] class NestedInstances4 extends NestedInstances5 {
  implicit def catsDataApplicativeErrorForNested[F[_], G[_], E](
    implicit F: ApplicativeError[F, E],
    GG: Applicative[G]
  ): ApplicativeError[({ type λ[α$] = Nested[F, G, α$] })#λ, E] =
    new NestedApplicativeError[F, G, E] {
      val G: Applicative[G] = Applicative[G]

      val AEF: ApplicativeError[F, E] = ApplicativeError[F, E]
    }

}

sealed abstract private[data] class NestedInstances5 extends NestedInstances6 {
  implicit def catsDataCommutativeApplicativeForNestedContravariant[F[_]: CommutativeApplicative, G[_]: CommutativeApplicative]
    : CommutativeApplicative[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedApplicative[F, G] with CommutativeApplicative[({ type λ[α$] = Nested[F, G, α$] })#λ] {
      val FG: Applicative[({ type λ[α] = F[G[α]] })#λ] = Applicative[F].compose[G]
    }

  implicit def catsDataMonoidKForNested[F[_]: MonoidK, G[_]]: MonoidK[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedMonoidK[F, G] {
      val FG: MonoidK[({ type λ[α] = F[G[α]] })#λ] = MonoidK[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances6 extends NestedInstances7 {
  implicit def catsDataCommutativeApplyForNestedContravariant[F[_]: CommutativeApply, G[_]: CommutativeApply]
    : CommutativeApply[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedApply[F, G] with CommutativeApply[({ type λ[α$] = Nested[F, G, α$] })#λ] {
      val FG: Apply[({ type λ[α] = F[G[α]] })#λ] = Apply[F].compose[G]
    }

  implicit def catsDataSemigroupKForNested[F[_]: SemigroupK, G[_]]: SemigroupK[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedSemigroupK[F, G] {
      val FG: SemigroupK[({ type λ[α] = F[G[α]] })#λ] = SemigroupK[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances7 extends NestedInstances8 {
  implicit def catsDataApplicativeForNested[F[_]: Applicative, G[_]: Applicative]
    : Applicative[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedApplicative[F, G] {
      val FG: Applicative[({ type λ[α] = F[G[α]] })#λ] = Applicative[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances8 extends NestedInstances9 {
  implicit def catsDataApplyForNested[F[_]: Apply, G[_]: Apply]: Apply[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedApply[F, G] {
      val FG: Apply[({ type λ[α] = F[G[α]] })#λ] = Apply[F].compose[G]
    }

  implicit def catsDataDistributiveForNested[F[_]: Distributive, G[_]: Distributive]
    : Distributive[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedDistributive[F, G] {
      val FG: Distributive[({ type λ[α] = F[G[α]] })#λ] = Distributive[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances9 extends NestedInstances10 {
  implicit def catsDataInvariantSemigroupalApplyForNested[F[_]: InvariantSemigroupal, G[_]: Apply]
    : InvariantSemigroupal[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedInvariantSemigroupalApply[F, G] {
      val FG: InvariantSemigroupal[({ type λ[α] = F[G[α]] })#λ] = InvariantSemigroupal[F].composeApply[G]
    }
}

sealed abstract private[data] class NestedInstances10 extends NestedInstances11 {
  implicit def catsDataFunctorForNested[F[_]: Functor, G[_]: Functor]: Functor[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedFunctor[F, G] {
      val FG: Functor[({ type λ[α] = F[G[α]] })#λ] = Functor[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances11 extends NestedInstances12 {
  implicit def catsDataInvariantForNested[F[_]: Invariant, G[_]: Invariant]
    : Invariant[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedInvariant[F, G] {
      val FG: Invariant[({ type λ[α] = F[G[α]] })#λ] = Invariant[F].compose[G]
    }
}

sealed abstract private[data] class NestedInstances12 extends NestedInstances13 {
  implicit def catsDataInvariantForCovariantNested[F[_]: Invariant, G[_]: Functor]
    : Invariant[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedInvariant[F, G] {
      val FG: Invariant[({ type λ[α] = F[G[α]] })#λ] = Invariant[F].composeFunctor[G]
    }
}

sealed abstract private[data] class NestedInstances13 {
  implicit def catsDataInvariantForNestedContravariant[F[_]: Invariant, G[_]: Contravariant]
    : Invariant[({ type λ[α$] = Nested[F, G, α$] })#λ] =
    new NestedInvariant[F, G] {
      val FG: Invariant[({ type λ[α] = F[G[α]] })#λ] = Invariant[F].composeContravariant[G]
    }
}

private[data] trait NestedInvariant[F[_], G[_]] extends Invariant[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: Invariant[({ type λ[α] = F[G[α]] })#λ]

  override def imap[A, B](fga: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(FG.imap(fga.value)(f)(g))
}

private[data] trait NestedFunctor[F[_], G[_]]
    extends Functor[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedInvariant[F, G] {
  override def FG: Functor[({ type λ[α] = F[G[α]] })#λ]

  override def map[A, B](fga: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
    Nested(FG.map(fga.value)(f))
}

private[data] trait NestedApply[F[_], G[_]]
    extends Apply[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedFunctor[F, G] {
  override def FG: Apply[({ type λ[α] = F[G[α]] })#λ]

  override def ap[A, B](fgf: Nested[F, G, A => B])(fga: Nested[F, G, A]): Nested[F, G, B] =
    Nested(FG.ap(fgf.value)(fga.value))

  override def product[A, B](fga: Nested[F, G, A], fgb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fga.value, fgb.value))
}

private[data] trait NestedApplicative[F[_], G[_]]
    extends Applicative[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedApply[F, G] {
  def FG: Applicative[({ type λ[α] = F[G[α]] })#λ]

  def pure[A](x: A): Nested[F, G, A] = Nested(FG.pure(x))
}

abstract private[data] class NestedApplicativeError[F[_], G[_], E]
    extends ApplicativeError[({ type λ[α$] = Nested[F, G, α$] })#λ, E]
    with NestedApplicative[F, G] {
  def G: Applicative[G]
  def AEF: ApplicativeError[F, E]

  def FG: Applicative[({ type λ[α] = F[G[α]] })#λ] = AEF.compose[G](G)

  def raiseError[A](e: E): Nested[F, G, A] = Nested(AEF.map(AEF.raiseError[A](e))(G.pure))

  def handleErrorWith[A](fa: Nested[F, G, A])(f: E => Nested[F, G, A]): Nested[F, G, A] =
    Nested(AEF.handleErrorWith(fa.value)(e => f(e).value))

}

private[data] trait NestedSemigroupK[F[_], G[_]] extends SemigroupK[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: SemigroupK[({ type λ[α] = F[G[α]] })#λ]

  def combineK[A](x: Nested[F, G, A], y: Nested[F, G, A]): Nested[F, G, A] = Nested(FG.combineK(x.value, y.value))
}

private[data] trait NestedMonoidK[F[_], G[_]]
    extends MonoidK[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedSemigroupK[F, G] {
  def FG: MonoidK[({ type λ[α] = F[G[α]] })#λ]

  def empty[A]: Nested[F, G, A] = Nested(FG.empty[A])
}

private[data] trait NestedAlternative[F[_], G[_]]
    extends Alternative[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedApplicative[F, G]
    with NestedMonoidK[F, G] {
  def FG: Alternative[({ type λ[α] = F[G[α]] })#λ]
}

private[data] trait NestedFoldable[F[_], G[_]] extends Foldable[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: Foldable[({ type λ[α] = F[G[α]] })#λ]

  def foldLeft[A, B](fga: Nested[F, G, A], b: B)(f: (B, A) => B): B =
    FG.foldLeft(fga.value, b)(f)

  def foldRight[A, B](fga: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.foldRight(fga.value, lb)(f)
}

private[data] trait NestedTraverse[F[_], G[_]]
    extends Traverse[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedFoldable[F, G]
    with NestedFunctor[F, G] {
  def FG: Traverse[({ type λ[α] = F[G[α]] })#λ]

  override def traverse[H[_]: Applicative, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Applicative[H].map(FG.traverse(fga.value)(f))(Nested(_))
}

private[data] trait NestedDistributive[F[_], G[_]]
    extends Distributive[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedFunctor[F, G] {
  def FG: Distributive[({ type λ[α] = F[G[α]] })#λ]

  def distribute[H[_]: Functor, A, B](ha: H[A])(f: A => Nested[F, G, B]): Nested[F, G, H[B]] =
    Nested(FG.distribute(ha) { a =>
      f(a).value
    })
}

private[data] trait NestedReducible[F[_], G[_]]
    extends Reducible[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedFoldable[F, G] {
  def FG: Reducible[({ type λ[α] = F[G[α]] })#λ]

  def reduceLeftTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (B, A) => B): B =
    FG.reduceLeftTo(fga.value)(f)(g)

  def reduceRightTo[A, B](fga: Nested[F, G, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    FG.reduceRightTo(fga.value)(f)(g)
}

private[data] trait NestedNonEmptyTraverse[F[_], G[_]]
    extends NonEmptyTraverse[({ type λ[α$] = Nested[F, G, α$] })#λ]
    with NestedTraverse[F, G]
    with NestedReducible[F, G] {
  def FG: NonEmptyTraverse[({ type λ[α] = F[G[α]] })#λ]

  override def nonEmptyTraverse[H[_]: Apply, A, B](fga: Nested[F, G, A])(f: A => H[B]): H[Nested[F, G, B]] =
    Apply[H].map(FG.nonEmptyTraverse(fga.value)(f))(Nested(_))
}

private[data] trait NestedContravariant[F[_], G[_]] extends Contravariant[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: Contravariant[({ type λ[α] = F[G[α]] })#λ]

  override def contramap[A, B](fga: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(FG.contramap(fga.value)(f))
}

private[data] trait NestedContravariantMonoidal[F[_], G[_]]
    extends ContravariantMonoidal[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: ContravariantMonoidal[({ type λ[α] = F[G[α]] })#λ]

  def unit: Nested[F, G, Unit] = Nested(FG.unit)

  def contramap[A, B](fa: Nested[F, G, A])(f: B => A): Nested[F, G, B] =
    Nested(FG.contramap(fa.value)(f))

  def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fa.value, fb.value))
}

private[data] trait NestedInvariantSemigroupalApply[F[_], G[_]]
    extends InvariantSemigroupal[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  def FG: InvariantSemigroupal[({ type λ[α] = F[G[α]] })#λ]

  def imap[A, B](fa: Nested[F, G, A])(f: A => B)(g: B => A): Nested[F, G, B] =
    Nested(FG.imap(fa.value)(f)(g))

  def product[A, B](fa: Nested[F, G, A], fb: Nested[F, G, B]): Nested[F, G, (A, B)] =
    Nested(FG.product(fa.value, fb.value))
}

abstract private[data] class NestedFunctorFilter[F[_], G[_]]
    extends FunctorFilter[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  implicit val F: Functor[F]

  implicit val G: FunctorFilter[G]

  def functor: Functor[({ type λ[α$] = Nested[F, G, α$] })#λ] = Nested.catsDataFunctorForNested(F, G.functor)

  def mapFilter[A, B](fa: Nested[F, G, A])(f: (A) => Option[B]): Nested[F, G, B] =
    Nested[F, G, B](F.map(fa.value)(G.mapFilter(_)(f)))

  override def collect[A, B](fa: Nested[F, G, A])(f: PartialFunction[A, B]): Nested[F, G, B] =
    Nested[F, G, B](F.map(fa.value)(G.collect(_)(f)))

  override def flattenOption[A](fa: Nested[F, G, Option[A]]): Nested[F, G, A] =
    Nested[F, G, A](F.map(fa.value)(G.flattenOption))

  override def filter[A](fa: Nested[F, G, A])(f: (A) => Boolean): Nested[F, G, A] =
    Nested[F, G, A](F.map(fa.value)(G.filter(_)(f)))
}

abstract private[data] class NestedTraverseFilter[F[_], G[_]]
    extends NestedFunctorFilter[F, G]
    with TraverseFilter[({ type λ[α$] = Nested[F, G, α$] })#λ] {
  implicit val F: Traverse[F]

  implicit val G: TraverseFilter[G]

  def traverse: Traverse[({ type λ[α$] = Nested[F, G, α$] })#λ] = Nested.catsDataTraverseForNested(F, G.traverse)

  override def filterA[H[_], A](
    fa: Nested[F, G, A]
  )(f: A => H[Boolean])(implicit H: Applicative[H]): H[Nested[F, G, A]] =
    H.map(F.traverse(fa.value)(G.filterA[H, A](_)(f)))(Nested[F, G, A])

  def traverseFilter[H[_], A, B](
    fga: Nested[F, G, A]
  )(f: A => H[Option[B]])(implicit H: Applicative[H]): H[Nested[F, G, B]] =
    H.map(F.traverse[H, G[A], G[B]](fga.value)(ga => G.traverseFilter(ga)(f)))(Nested[F, G, B])
}
