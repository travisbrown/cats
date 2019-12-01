package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative Monad.
 *
 * Further than a Monad, which just allows composition of dependent effectful functions,
 * in a Commutative Monad those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeMonadLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeMonad for ${F}")
@typeclass trait CommutativeMonad[F[_]] extends Monad[F] with CommutativeFlatMap[F] with CommutativeApplicative[F]
