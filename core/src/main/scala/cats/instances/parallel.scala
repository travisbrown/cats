package cats
package instances

import cats.data._
import cats.kernel.Semigroup

private[instances] trait ParallelInstances1 {
  @deprecated("Use EitherT.catsDataParallelForEitherTWithSequentialEffect", "2.0.0")
  def catsParallelForEitherTNestedValidated[M[_]: Monad, E: Semigroup]
    : Parallel.Aux[({ type λ[α$] = EitherT[M, E, α$] })#λ,
                   ({ type λ[α$] = Nested[M, ({ type λ[α$$] = Validated[E, α$$] })#λ, α$] })#λ] =
    EitherT.catsDataParallelForEitherTWithSequentialEffect
}
