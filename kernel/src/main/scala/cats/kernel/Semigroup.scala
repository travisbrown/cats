package cats.kernel

import scala.annotation.tailrec
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.{specialized => sp}
import compat.scalaVersionSpecific._

/**
 * A semigroup is any set `A` with an associative operation (`combine`).
 */
trait Semigroup[@sp(Int, Long, Float, Double) A] extends Any with Serializable {

  /**
   * Associative operation which combines two values.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.option._
   *
   * scala> Semigroup[String].combine("Hello ", "World!")
   * res0: String = Hello World!
   *
   * scala> Semigroup[Option[Int]].combine(None, Some(1))
   * res1: Option[Int] = Some(1)
   * }}}
   */
  def combine(x: A, y: A): A

  /**
   * Return `a` combined with itself `n` times.
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.int._
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[Int].combineN(1, 10)
   * res0: Int = 10
   *
   * scala> Semigroup[String].combineN("ha", 3)
   * res1: String = hahaha
   * }}}
   */
  def combineN(a: A, n: Int): A =
    if (n <= 0) throw new IllegalArgumentException("Repeated combining for semigroups must have n > 0")
    else repeatedCombineN(a, n)

  /**
   * Return `a` combined with itself more than once.
   */
  protected[this] def repeatedCombineN(a: A, n: Int): A = {
    @tailrec def loop(b: A, k: Int, extra: A): A =
      if (k == 1) combine(b, extra)
      else {
        val x = if ((k & 1) == 1) combine(b, extra) else extra
        loop(combine(b, b), k >>> 1, x)
      }
    if (n == 1) a else loop(a, n - 1, a)
  }

  /**
   * Given a sequence of `as`, combine them and return the total.
   *
   * If the sequence is empty, returns None. Otherwise, returns Some(total).
   *
   * Example:
   * {{{
   * scala> import cats.kernel.instances.string._
   *
   * scala> Semigroup[String].combineAllOption(List("One ", "Two ", "Three"))
   * res0: Option[String] = Some(One Two Three)
   *
   * scala> Semigroup[String].combineAllOption(List.empty)
   * res1: Option[String] = None
   * }}}
   */
  def combineAllOption(as: IterableOnce[A]): Option[A] =
    as.reduceOption(combine)
}

abstract class SemigroupFunctions[S[T] <: Semigroup[T]] {
  def combine[@sp(Int, Long, Float, Double) A](x: A, y: A)(implicit ev: S[A]): A =
    ev.combine(x, y)

  def maybeCombine[@sp(Int, Long, Float, Double) A](ox: Option[A], y: A)(implicit ev: S[A]): A =
    ox match {
      case Some(x) => ev.combine(x, y)
      case None    => y
    }

  def maybeCombine[@sp(Int, Long, Float, Double) A](x: A, oy: Option[A])(implicit ev: S[A]): A =
    oy match {
      case Some(y) => ev.combine(x, y)
      case None    => x
    }

  def isCommutative[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[CommutativeSemigroup[_]]

  def isIdempotent[A](implicit ev: S[A]): Boolean =
    ev.isInstanceOf[Band[_]]

  def combineN[@sp(Int, Long, Float, Double) A](a: A, n: Int)(implicit ev: S[A]): A =
    ev.combineN(a, n)

  def combineAllOption[A](as: IterableOnce[A])(implicit ev: S[A]): Option[A] =
    ev.combineAllOption(as)
}

object Semigroup
    extends SemigroupFunctions[Semigroup]
    with instances.TupleCommutativeGroupInstances
    with SemilatticeInstances {

  /**
   * Access an implicit `Semigroup[A]`.
   */
  @inline final def apply[A](implicit ev: Semigroup[A]): Semigroup[A] = ev

  /**
   * Create a `Semigroup` instance from the given function.
   */
  @inline def instance[A](cmb: (A, A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = cmb(x, y)
  }

  implicit def catsKernelBoundedSemilatticeForBitSet: BoundedSemilattice[BitSet] =
    cats.kernel.instances.bitSet.catsKernelStdSemilatticeForBitSet
  implicit def catsKernelInstancesForUnit: BoundedSemilattice[Unit] with CommutativeGroup[Unit] =
    cats.kernel.instances.unit.catsKernelStdAlgebraForUnit
  implicit def catsKernelCommutativeGroupForByte: CommutativeGroup[Byte] =
    cats.kernel.instances.byte.catsKernelStdGroupForByte
  implicit def catsKernelCommutativeGroupForShort: CommutativeGroup[Short] =
    cats.kernel.instances.short.catsKernelStdGroupForShort
  implicit def catsKernelCommutativeGroupForInt: CommutativeGroup[Int] =
    cats.kernel.instances.int.catsKernelStdGroupForInt
  implicit def catsKernelCommutativeGroupForLong: CommutativeGroup[Long] =
    cats.kernel.instances.long.catsKernelStdGroupForLong
  implicit def catsKernelCommutativeGroupForBigInt: CommutativeGroup[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdGroupForBigInt
  implicit def catsKernelCommutativeGroupForBigDecimal: CommutativeGroup[BigDecimal] =
    cats.kernel.instances.bigDecimal.catsKernelStdGroupForBigDecimal
  implicit def catsKernelCommutativeGroupForDuration: CommutativeGroup[Duration] =
    cats.kernel.instances.duration.catsKernelStdGroupForDuration
  implicit def catsKernelCommutativeGroupForFiniteDuration: CommutativeGroup[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdGroupForFiniteDuration
  implicit def catsKernelCommutativeGroupForDouble: CommutativeGroup[Double] =
    cats.kernel.instances.double.catsKernelStdGroupForDouble
  implicit def catsKernelCommutativeGroupForFloat: CommutativeGroup[Float] =
    cats.kernel.instances.float.catsKernelStdGroupForFloat

  implicit def catsKernelMonoidForString: Monoid[String] = cats.kernel.instances.string.catsKernelStdMonoidForString

  implicit def catsKernelMonoidForOption[A: Semigroup]: Monoid[Option[A]] =
    cats.kernel.instances.option.catsKernelStdMonoidForOption[A]
  implicit def catsKernelMonoidForList[A]: Monoid[List[A]] = cats.kernel.instances.list.catsKernelStdMonoidForList[A]
  implicit def catsKernelMonoidForVector[A]: Monoid[Vector[A]] =
    cats.kernel.instances.vector.catsKernelStdMonoidForVector[A]
  implicit def catsKernelMonoidForStream[A]: Monoid[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdMonoidForStream[A]
  implicit def catsKernelMonoidForQueue[A]: Monoid[Queue[A]] =
    cats.kernel.instances.queue.catsKernelStdMonoidForQueue[A]

  implicit def catsKernelBoundedSemilatticeForSet[A]: Monoid[Set[A]] =
    cats.kernel.instances.set.catsKernelStdSemilatticeForSet[A]
  implicit def catsKernelBoundedSemilatticeForSortedSet[A: Order]: Monoid[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdBoundedSemilatticeForSortedSet[A]

  implicit def catsKernelGroupForFunction0[A: Group]: Group[() => A] =
    cats.kernel.instances.function.catsKernelGroupForFunction0[A]
  implicit def catsKernelGroupForFunction1[A, B: Group]: Group[A => B] =
    cats.kernel.instances.function.catsKernelGroupForFunction1[A, B]

  implicit def catsKernelCommutativeMonoidForMap[K, V: CommutativeSemigroup]: CommutativeMonoid[Map[K, V]] =
    cats.kernel.instances.map.catsKernelStdCommutativeMonoidForMap[K, V]
  implicit def catsKernelCommutativeMonoidForSortedMap[K: Order, V: CommutativeSemigroup]
    : CommutativeMonoid[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdCommutativeMonoidForSortedMap[K, V]
}

private[kernel] trait SemilatticeInstances extends MonoidInstances {
  implicit def catsKernelSemilatticeForFunction0[A: Semilattice]: Semilattice[() => A] =
    cats.kernel.instances.function.catsKernelSemilatticeForFunction0[A]
  implicit def catsKernelSemilatticeForFunction1[A, B: Semilattice]: Semilattice[A => B] =
    cats.kernel.instances.function.catsKernelSemilatticeForFunction1[A, B]
}

private[kernel] trait MonoidInstances extends CommutativeSemigroupInstances {
  implicit def catsKernelMonoidForFunction0[A: Monoid]: Monoid[() => A] =
    cats.kernel.instances.function.catsKernelMonoidForFunction0[A]
  implicit def catsKernelMonoidForSortedMap[K: Order, V: Semigroup]: Monoid[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdMonoidForSortedMap[K, V]
}

private[kernel] trait CommutativeSemigroupInstances extends BandInstances {
  implicit def catsKernelCommutativeSemigroupForFunction0[A: CommutativeSemigroup]: CommutativeSemigroup[() => A] =
    cats.kernel.instances.function.catsKernelCommutativeSemigroupForFunction0[A]
}

private[kernel] trait BandInstances extends SemigroupInstances {
  implicit def catsKernelBandForFunction0[A: Band]: Band[() => A] =
    cats.kernel.instances.function.catsKernelBandForFunction0[A]
}

private[kernel] trait SemigroupInstances {
  implicit def catsKernelSemigroupForFunction0[A: Semigroup]: Semigroup[() => A] =
    cats.kernel.instances.function.catsKernelSemigroupForFunction0[A]
  implicit def catsKernelSemigroupForFunction1[A, B: Semigroup]: Semigroup[A => B] =
    cats.kernel.instances.function.catsKernelSemigroupForFunction1[A, B]
}
