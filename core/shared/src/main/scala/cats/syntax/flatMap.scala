package cats
package syntax

trait FlatMapSyntax1 {
  implicit def flatMapSyntaxU[FA](fa: FA)(implicit U: Unapply[FlatMap, FA]): FlatMapOps[U.M, U.A] =
    new FlatMapOps[U.M, U.A](U.subst(fa))(U.TC)
}

trait FlatMapSyntax extends FlatMapSyntax1 {
  implicit def flatMapSyntax[F[_]: FlatMap, A](fa: F[A]): FlatMapOps[F, A] =
    new FlatMapOps(fa)

  implicit def flattenSyntax[F[_]: FlatMap, A](ffa: F[F[A]]): FlattenOps[F, A] =
    new FlattenOps[F, A](ffa)

  implicit def ifMSyntax[F[_]: FlatMap](fa: F[Boolean]): IfMOps[F] =
    new IfMOps[F](fa)
}

final class FlatMapOps[F[_], A](fa: F[A])(implicit F: FlatMap[F]) {
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  def mproduct[B](f: A => F[B]): F[(A, B)] = F.mproduct(fa)(f)
  def >>=[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)

  /** Alias for [[followedBy]]. */
  @inline final def >> [B](fb: F[B]): F[B] = followedBy(fb)

  /** Sequentially compose two actions, discarding any value produced by the first. */
  def followedBy[B](fb: F[B]): F[B] = F.flatMap(fa)(_ => fb)

  /**
   * Sequentially compose two actions, discarding any value produced by the first. This variant of
   * [[followedBy]] also lets you define the evaluation strategy of the second action. For instance
   * you can evaluate it only ''after'' the first action has finished:
   *
   * {{{
   *   fa.followedByEval(later(fb))
   * }}}
   */
  def followedByEval[B](fb: Eval[F[B]]): F[B] = F.flatMap(fa)(_ => fb.value)

}

final class FlattenOps[F[_], A](ffa: F[F[A]])(implicit F: FlatMap[F]) {
  def flatten: F[A] = F.flatten(ffa)
}

final class IfMOps[F[_]](fa: F[Boolean])(implicit F: FlatMap[F]) {
  def ifM[B](ifTrue: => F[B], ifFalse: => F[B]): F[B] = F.ifM(fa)(ifTrue, ifFalse)
}
