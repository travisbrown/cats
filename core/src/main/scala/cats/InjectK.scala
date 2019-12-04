package cats

import cats.arrow.FunctionK
import cats.data.EitherK

/**
 * InjectK is a type class providing an injection from type
 * constructor `F` into type constructor `G`. An injection is a
 * functor transformation `inj` which does not destroy any
 * information: for every `ga: G[A]` there is at most one `fa: F[A]`
 * such that `inj(fa) = ga`.
 *
 * Because of this all injections admit partial inverses `prj` which
 * pair a value `ga: G[A]` back with a single value `fa: F[A]`.
 *
 * The behavior of the default instances for the InjectK type class
 * are described thoroughly in "Data types a la carte" (Swierstra
 * 2008).
 *
 * @note Prior to cats 1.0, InjectK was known as [[Inject]].
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/publications/2008-jfp.pdf]]
 * @see [[Inject]] for injection for `Either`
 */
abstract class InjectK[F[_], G[_]] {
  def inj: FunctionK[F, G]

  def prj: FunctionK[G, ({ type λ[α] = Option[F[α]] })#λ]

  final def apply[A](fa: F[A]): G[A] = inj(fa)

  final def unapply[A](ga: G[A]): Option[F[A]] = prj(ga)
}

sealed abstract private[cats] class InjectKInstances {
  implicit def catsReflexiveInjectKInstance[F[_]]: InjectK[F, F] =
    new InjectK[F, F] {
      val inj = FunctionK.id[F]

      val prj = new FunctionK[F, ({ type λ[α] = Option[F[α]] })#λ] {
        def apply[A$](a$ : F[A$]): Option[F[A$]] = Some(a$)
      }
    }

  implicit def catsLeftInjectKInstance[F[_], G[_]]: InjectK[F, ({ type λ[α$] = EitherK[F, G, α$] })#λ] =
    new InjectK[F, ({ type λ[α$] = EitherK[F, G, α$] })#λ] {
      val inj = new FunctionK[F, ({ type λ[α$] = EitherK[F, G, α$] })#λ] {
        def apply[A$](a$ : F[A$]): EitherK[F, G, A$] = EitherK.leftc(a$)
      }

      val prj = new FunctionK[({ type λ[α$] = EitherK[F, G, α$] })#λ, ({ type λ[α] = Option[F[α]] })#λ] {
        def apply[A$](a$ : EitherK[F, G, A$]): Option[F[A$]] = a$.run.left.toOption
      }
    }

  implicit def catsRightInjectKInstance[F[_], G[_], H[_]](
    implicit I: InjectK[F, G]
  ): InjectK[F, ({ type λ[α$] = EitherK[H, G, α$] })#λ] =
    new InjectK[F, ({ type λ[α$] = EitherK[H, G, α$] })#λ] {
      val inj = new FunctionK[G, ({ type λ[α$] = EitherK[H, G, α$] })#λ] {
        def apply[A$](a$ : G[A$]): EitherK[H, G, A$] = EitherK.rightc(a$)
      }.compose(I.inj)

      val prj = new FunctionK[({ type λ[α$] = EitherK[H, G, α$] })#λ, ({ type λ[α] = Option[F[α]] })#λ] {
        def apply[A$](a$ : EitherK[H, G, A$]): Option[F[A$]] = a$.run.toOption.flatMap(I.prj(_))
      }
    }
}

object InjectK extends InjectKInstances {
  def apply[F[_], G[_]](implicit I: InjectK[F, G]): InjectK[F, G] = I
}
