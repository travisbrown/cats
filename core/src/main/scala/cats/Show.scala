package cats

import java.util.UUID
import scala.collection.immutable.{BitSet, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
trait Show[T] extends Show.ContravariantShow[T]

/**
 * Hand rolling the type class boilerplate due to scala/bug#6260 and scala/bug#10458
 */
object Show {

  def apply[A](implicit instance: Show[A]): Show[A] = instance

  trait ContravariantShow[-T] extends Serializable {
    def show(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: Show[A]
    def self: A
    def show: String = typeClassInstance.show(self)
  }

  trait ToShowOps {
    implicit def toShow[A](target: A)(implicit tc: Show[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  final case class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: ContravariantShow[A]): Shown = Shown(z.show(x))
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc.s(args: _*)
  }

  implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B]((fa.show _).compose(f))
  }

  implicit val catsShowForUnit: Show[Unit] = Show.fromToString[Unit]
  implicit val catsShowForBoolean: Show[Boolean] = Show.fromToString[Boolean]
  implicit val catsShowForByte: Show[Byte] = Show.fromToString[Byte]
  implicit val catsShowForShort: Show[Short] = Show.fromToString[Short]
  implicit val catsShowForInt: Show[Int] = Show.fromToString[Int]
  implicit val catsShowForLong: Show[Long] = Show.fromToString[Long]
  implicit val catsShowForFloat: Show[Float] = Show.fromToString[Float]
  implicit val catsShowForDouble: Show[Double] = Show.fromToString[Double]
  implicit val catsShowForBigInt: Show[BigInt] = Show.fromToString[BigInt]
  implicit val catsShowForBigDecimal: Show[BigDecimal] = Show.fromToString[BigDecimal]
  implicit val catsShowForChar: Show[Char] = Show.fromToString[Char]
  implicit val catsShowForSymbol: Show[Symbol] = Show.fromToString[Symbol]
  implicit val catsShowForString: Show[String] = Show.fromToString[String]
  implicit val catsShowForUUID: Show[UUID] = Show.fromToString[UUID]
  implicit val catsShowForDuration: Show[Duration] = Show.fromToString[Duration]
  implicit val catsShowForFiniteDuration: Show[FiniteDuration] = Show.fromToString[FiniteDuration]
  implicit val catsShowForBitSet: Show[BitSet] = Show.fromToString[BitSet]

  implicit def catsShowForOption[A: Show]: Show[Option[A]] = cats.instances.option.catsStdShowForOption[A]
  implicit def catsShowForList[A: Show]: Show[List[A]] = cats.instances.list.catsStdShowForList[A]
  implicit def catsShowForVector[A: Show]: Show[Vector[A]] = cats.instances.vector.catsStdShowForVector[A]
  implicit def catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]
  implicit def catsShowForEither[A: Show, B: Show]: Show[Either[A, B]] =
    cats.instances.either.catsStdShowForEither[A, B]
  implicit def catsShowForTuple2[A: Show, B: Show]: Show[(A, B)] = cats.instances.tuple.catsStdShowForTuple2[A, B]
  implicit def catsShowForSet[A: Show]: Show[Set[A]] = cats.instances.set.catsStdShowForSet[A]
  implicit def catsShowForMap[K: Show, V: Show]: Show[Map[K, V]] = cats.instances.map.catsStdShowForMap[K, V]
  implicit def catsShowForSortedSet[A: Show]: Show[SortedSet[A]] = cats.instances.sortedSet.catsStdShowForSortedSet[A]
  implicit def catsShowForSortedMap[K: Order: Show, V: Show]: Show[SortedMap[K, V]] =
    cats.instances.sortedMap.catsStdShowForSortedMap[K, V]
}
