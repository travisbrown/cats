package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
 * Commutative FlatMap.
 *
 * Further than a FlatMap, which just allows composition of dependent effectful functions,
 * in a Commutative FlatMap those functions can be composed in any order, which guarantees
 * that their effects do not interfere.
 *
 * Must obey the laws defined in cats.laws.CommutativeFlatMapLaws.
 */
@implicitNotFound("Could not find an instance of CommutativeFlatMap for ${F}")
@typeclass trait CommutativeFlatMap[F[_]] extends FlatMap[F] with CommutativeApply[F]
