package cats

import cats.kernel.CommutativeMonoid
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.util.Try
import simulacrum.typeclass

/**
 * `UnorderedFoldable` is like a `Foldable` for unordered containers.
 */
@typeclass trait UnorderedFoldable[F[_]] {

  def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  def unorderedFold[A: CommutativeMonoid](fa: F[A]): A =
    unorderedFoldMap(fa)(identity)

  /**
   * Returns true if there are no elements. Otherwise false.
   */
  def isEmpty[A](fa: F[A]): Boolean =
    !nonEmpty(fa)

  def nonEmpty[A](fa: F[A]): Boolean =
    exists(fa)(Function.const(true))

  /**
   * Check whether at least one element satisfies the predicate.
   *
   * If there are no elements, the result is `false`.
   */
  def exists[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.orEvalMonoid).value

  /**
   * Check whether all elements satisfy the predicate.
   *
   * If there are no elements, the result is `true`.
   */
  def forall[A](fa: F[A])(p: A => Boolean): Boolean =
    unorderedFoldMap(fa)(a => Eval.later(p(a)))(UnorderedFoldable.andEvalMonoid).value

  /**
   * The size of this UnorderedFoldable.
   *
   * This is overridden in structures that have more efficient size implementations
   * (e.g. Vector, Set, Map).
   *
   * Note: will not terminate for infinite-sized collections.
   */
  def size[A](fa: F[A]): Long = unorderedFoldMap(fa)(_ => 1L)
}

object UnorderedFoldable extends ScalaVersionSpecificTraverseInstances {
  private val orEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.False

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => Eval.True
        case false => ly
      }
  }

  private val andEvalMonoid: CommutativeMonoid[Eval[Boolean]] = new CommutativeMonoid[Eval[Boolean]] {
    val empty: Eval[Boolean] = Eval.True

    def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] =
      lx.flatMap {
        case true  => ly
        case false => Eval.False
      }
  }

  implicit def catsNonEmptyTraverseForId: NonEmptyTraverse[Id] = catsInstancesForId
  implicit def catsTraverseForOption: Traverse[Option] = cats.instances.option.catsStdInstancesForOption
  implicit def catsTraverseForList: Traverse[List] = cats.instances.list.catsStdInstancesForList
  implicit def catsTraverseForVector: Traverse[Vector] = cats.instances.vector.catsStdInstancesForVector
  implicit def catsUnorderedTraverseForSet: UnorderedTraverse[Set] = cats.instances.set.catsStdInstancesForSet
  implicit def catsFoldableForSortedSet: Foldable[SortedSet] = cats.instances.sortedSet.catsStdInstancesForSortedSet
  implicit def catsTraverseForSortedMap[K: Order]: Traverse[SortedMap[K, *]] =
    cats.instances.sortedMap.catsStdInstancesForSortedMap[K]

  implicit def catsUnorderedTraverseForMap[K]: UnorderedTraverse[Map[K, *]] =
    cats.instances.map.catsStdInstancesForMap[K]

  implicit def catsFoldableForEither[A]: Foldable[Either[A, *]] = cats.instances.either.catsStdInstancesForEither[A]
  implicit def catsFoldableForTuple[A]: Traverse[(A, *)] = cats.instances.tuple.catsStdInstancesForTuple2[A]

  implicit def catsFoldableForTry: Foldable[Try] = cats.instances.try_.catsStdInstancesForTry
}
