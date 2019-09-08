package cats.kernel

private[kernel] trait ScalaVersionSpecificOrderInstances {
  @deprecated("3.0.0", "Use catsKernelOrderForLazyList")
  implicit def catsKernelOrderForStream[A: Order]: Order[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdOrderForStream[A]

  implicit def catsKernelOrderForLazyList[A: Order]: Order[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdOrderForLazyList[A]
}

private[kernel] trait ScalaVersionSpecificPartialOrderInstances {
  @deprecated("3.0.0", "Use catsKernelPartialOrderForLazyList")
  implicit def catsKernelPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdPartialOrderForStream[A]

  implicit def catsKernelPartialOrderForLazyList[A: PartialOrder]: PartialOrder[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdPartialOrderForLazyList[A]
}

private[kernel] trait ScalaVersionSpecificHashInstances {
  @deprecated("3.0.0", "Use catsKernelHashForLazyList")
  implicit def catsKernelHashForStream[A: Hash]: Hash[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdHashForStream[A]

  implicit def catsKernelHashForLazyList[A: Hash]: Hash[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdHashForLazyList[A]
}

private[kernel] trait ScalaVersionSpecificEqInstances {
  @deprecated("3.0.0", "Use catsKernelEqForLazyList")
  implicit def catsKernelEqForStream[A: Eq]: Eq[Stream[A]] = cats.kernel.instances.stream.catsKernelStdEqForStream[A]

  implicit def catsKernelEqForLazyList[A: Eq]: Eq[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdEqForLazyList[A]
}

private[kernel] trait ScalaVersionSpecificMonoidInstances {
  @deprecated("3.0.0", "Use catsKernelMonoidForLazyList")
  implicit def catsKernelMonoidForStream[A]: Monoid[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdMonoidForStream[A]

  implicit def catsKernelMonoidForLazyList[A]: Monoid[LazyList[A]] =
    cats.kernel.instances.lazyList.catsKernelStdMonoidForLazyList[A]
}
