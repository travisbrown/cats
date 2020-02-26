package cats.kernel

import scala.deriving.{ArrayProduct, Mirror}
import scala.compiletime.{erasedValue, summonFrom}

private[kernel] trait BandDerivation
private[kernel] trait BoundedSemilatticeDerivation
private[kernel] trait CommutativeGroupDerivation
private[kernel] trait CommutativeMonoidDerivation
private[kernel] trait CommutativeSemigroupDerivation
private[kernel] trait EqDerivation {
  inline final def summonInstance[A]: Eq[A] = summonFrom[Eq[A]] {
    case instanceA: Eq[A] => instanceA
    case _: Mirror.Of[A] => derived[A]
  }

  inline final def summonInstances[T <: Tuple]: List[Eq[_]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: (t *: ts) => summonInstance[t] :: summonInstances[ts]
    }

  inline final def derived[A](given inline A: Mirror.Of[A]): Eq[A] =
    new Eq[A] {
      private[this] lazy val instances: List[Eq[_]] = summonInstances[A.MirroredElemTypes]

      def eqv(x: A, y: A): Boolean = inline A match {
        case m: Mirror.ProductOf[A] =>
          val iterX = x.asInstanceOf[Product].productIterator
          val iterY = y.asInstanceOf[Product].productIterator
          val iterI = instances.iterator
          var result = true

          while (result && iterX.hasNext) {
            val nextX = iterX.next()
            val nextY = iterY.next()
            val nextI = iterI.next().asInstanceOf[Eq[Any]]

            result = nextI.eqv(nextX, nextY)
          }

          result
        case m: Mirror.SumOf[A] =>
          val ordinalX = m.ordinal(x)
          val ordinalY = m.ordinal(y)

          if (ordinalX == ordinalY) {
            instances(ordinalX).asInstanceOf[Eq[Any]].eqv(x, y)
          } else {
            false
          }
      }
    }
}
private[kernel] trait GroupDerivation
private[kernel] trait HashDerivation
private[kernel] trait LowerBoundedDerivation
private[kernel] trait MonoidDerivation
private[kernel] trait OrderDerivation
private[kernel] trait PartialOrderDerivation
private[kernel] trait SemigroupDerivation {
  inline final def summonInstance[A]: Semigroup[A] = summonFrom[Semigroup[A]] {
    case instanceA: Semigroup[A] => instanceA
    case _: Mirror.Of[A] => derived[A]
  }

  inline final def summonInstances[T <: Tuple]: List[Semigroup[_]] =
    inline erasedValue[T] match {
      case _: Unit => Nil
      case _: (t *: ts) => summonInstance[t] :: summonInstances[ts]
    }

  inline final def derived[A](given inline A: Mirror.Of[A]): Semigroup[A] =
    new Semigroup[A] {
      private[this] lazy val instances: List[Semigroup[_]] = summonInstances[A.MirroredElemTypes]

      def combine(x: A, y: A): A = inline A match {
        case m: Mirror.ProductOf[A] =>
          val iterX = x.asInstanceOf[Product].productIterator
          val iterY = y.asInstanceOf[Product].productIterator
          val iterI = instances.iterator
          var result = new Array[AnyRef](instances.length)
          var i = 0

          while (iterX.hasNext) {
            val nextX = iterX.next()
            val nextY = iterY.next()
            val nextI = iterI.next().asInstanceOf[Semigroup[Any]]

            result(i) = nextI.combine(nextX, nextY).asInstanceOf[AnyRef]
            i += 1
          }

          m.fromProduct(new ArrayProduct(result))
      }
    }
}
private[kernel] trait SemilatticeDerivation
private[kernel] trait UpperBoundedDerivation
