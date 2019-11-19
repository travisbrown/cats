package cats

import cats.data.{ZipLazyList, ZipStream}

private[cats] trait ScalaVersionSpecificTraverseInstances {
  @deprecated("3.0.0", "Use catsTraverseForLazyList")
  implicit def catsTraverseForStream: Traverse[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsTraverseForLazyList: Traverse[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
}

private[cats] trait ScalaVersionSpecificShowInstances {
  @deprecated("3.0.0", "Use catsShowForLazyList")
  implicit def catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]

  implicit def catsShowForLazyList[A: Show]: Show[LazyList[A]] = cats.instances.lazyList.catsStdShowForLazyList[A]
}

private[cats] trait ScalaVersionSpecificSemigroupalInstances {
  @deprecated("3.0.0", "Use catsSemigroupalForLazyList")
  implicit def catsSemigroupalForStream: Semigroupal[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsSemigroupalForLazyList: Semigroupal[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
}

private[cats] trait ScalaVersionSpecificMonoidKInstances {
  @deprecated("3.0.0", "Use catsMonoidKForLazyList")
  implicit def catsMonoidKForStream: MonoidK[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsMonoidKForLazyList: MonoidK[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
}

private[cats] trait ScalaVersionSpecificParallelInstances {
  @deprecated("3.0.0", "Use catsParallelForLazyList")
  implicit def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.parallel.catsStdParallelForZipStream

  implicit def catsStdParallelForZipLazyList: Parallel.Aux[LazyList, ZipLazyList] =
    cats.instances.lazyList.catsStdParallelForLazyListZipLazyList
}

private[cats] trait ScalaVersionSpecificInvariantInstances {
  @deprecated("3.0.0", "Use catsInstancesForLazyList")
  implicit def catsInstancesForStream: Monad[Stream] with Alternative[Stream] with CoflatMap[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  implicit def catsInstancesForLazyList: Monad[LazyList] with Alternative[LazyList] with CoflatMap[LazyList] =
    cats.instances.lazyList.catsStdInstancesForLazyList
}

private[cats] trait ScalaVersionSpecificTraverseFilterInstances {
  @deprecated("3.0.0", "Use catsTraverseFilterForLazyList")
  implicit def catsTraverseFilterForStream: TraverseFilter[Stream] =
    cats.instances.stream.catsStdTraverseFilterForStream

  implicit def catsTraverseFilterForLazyList: TraverseFilter[LazyList] =
    cats.instances.lazyList.catsStdTraverseFilterForLazyList
}

private[cats] trait ScalaVersionSpecificAlignInstances {
  @deprecated("3.0.0", "Use catsTraverseFilterForLazyList")
  implicit def catsAlignForStream: Align[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  implicit def catsAlignForLazyList: Align[LazyList] =
    cats.instances.lazyList.catsStdInstancesForLazyList
}
