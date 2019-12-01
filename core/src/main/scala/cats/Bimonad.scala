package cats

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Bimonad for ${F}")
@typeclass trait Bimonad[F[_]] extends Monad[F] with Comonad[F]
