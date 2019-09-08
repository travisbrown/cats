package cats.kernel

private[kernel] trait ScalaVersionSpecificOrderInstances {
  implicit def catsKernelOrderForStream[A: Order]: Order[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdOrderForStream[A]
}

private[kernel] trait ScalaVersionSpecificPartialOrderInstances {
  implicit def catsKernelPartialOrderForStream[A: PartialOrder]: PartialOrder[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdPartialOrderForStream[A]
}

private[kernel] trait ScalaVersionSpecificHashInstances {
  implicit def catsKernelHashForStream[A: Hash]: Hash[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdHashForStream[A]
}

private[kernel] trait ScalaVersionSpecificEqInstances {
  implicit def catsKernelEqForStream[A: Eq]: Eq[Stream[A]] = cats.kernel.instances.stream.catsKernelStdEqForStream[A]
}

private[kernel] trait ScalaVersionSpecificMonoidInstances {
  implicit def catsKernelMonoidForStream[A]: Monoid[Stream[A]] =
    cats.kernel.instances.stream.catsKernelStdMonoidForStream[A]
}
