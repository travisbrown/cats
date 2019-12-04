package cats
package instances

import cats.data._
import cats.kernel.Semigroup
import cats.{NonEmptyParallel, Parallel}

trait ParallelInstances extends ParallelInstances1 {

  @deprecated("Use cats.instances.either.catsParallelForEitherAndValidated", "2.1.0")
  def catsParallelForEitherValidated[E: Semigroup]
    : Parallel.Aux[({ type λ[α$] = Either[E, α$] })#λ, ({ type λ[α$] = Validated[E, α$] })#λ] =
    cats.instances.either.catsParallelForEitherAndValidated[E]

  @deprecated("Use OptionT.catsDataParallelForOptionT", "2.0.0")
  def catsParallelForOptionTNestedOption[M[_]](
    implicit P: Parallel[M]
  ): Parallel.Aux[({ type λ[α$] = OptionT[M, α$] })#λ, ({ type λ[α$] = Nested[P.F, Option, α$] })#λ] =
    OptionT.catsDataParallelForOptionT[M]

  @deprecated("Use cats.instances.list.catsStdNonEmptyParallelForListZipList", "2.1.0")
  def catsStdNonEmptyParallelForZipList: NonEmptyParallel.Aux[List, ZipList] =
    cats.instances.list.catsStdNonEmptyParallelForListZipList

  @deprecated("Use cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector", "2.1.0")
  def catsStdNonEmptyParallelForZipVector: NonEmptyParallel.Aux[Vector, ZipVector] =
    cats.instances.vector.catsStdNonEmptyParallelForVectorZipVector

  @deprecated("Use cats.instances.stream.catsStdParallelForStreamZipStream", "2.1.0")
  def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.stream.catsStdParallelForStreamZipStream

  @deprecated("Use EitherT.catsDataParallelForEitherTWithParallelEffect", "2.0.0")
  def catsParallelForEitherTNestedParallelValidated[M[_], E: Semigroup](
    implicit P: Parallel[M]
  ): Parallel.Aux[({ type λ[α$] = EitherT[M, E, α$] })#λ,
                  ({ type λ[α$] = Nested[P.F, ({ type λ[α$$] = Validated[E, α$$] })#λ, α$] })#λ] =
    EitherT.catsDataParallelForEitherTWithParallelEffect[M, E]
}
