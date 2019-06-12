package baudrillard.symbolic

import shapeless.Witness


trait Unapply[A, X] {
  type Out[_]
  def reapply[X1](in: A, x1: X1): Out[X1]
}

object Unapply extends LowPriorityUnapply {

  type Aux[A, X, Out0[_]] = Unapply[A, X] { type Out[T] = Out0[T] }

  implicit def id[X]: Aux[X, X, ({type L[A] = A})#L] = new Unapply[X, X] {
    type Out[X1] = X1
    def reapply[X1](in: X, x1: X1): X1 = x1
  }

  implicit def recurse1[F[AA] <: Expr1[F, AA], A, X](implicit
    unapplyA: Unapply[A, X],
    applyF: Apply1[F]
  ): Aux[F[A], X, ({type L[N] = F[unapplyA.Out[N]]})#L] =
    new Unapply[F[A], X] {
      type Out[N] = F[unapplyA.Out[N]]
      def reapply[X1](in: F[A], x1: X1): F[unapplyA.Out[X1]] = applyF.apply(unapplyA.reapply(in.a, x1))
    }

  implicit def recurse2[F[AA, BB] <: Expr2[F, AA, BB], A, B, X](implicit
    unapplyA: Unapply[A, X],
    unapplyB: Unapply[B, X],
    applyF: Apply2[F]
  ): Aux[F[A, B], X, ({type L[N] = F[unapplyA.Out[N], unapplyB.Out[N]]})#L] =
    new Unapply[F[A, B], X] {
      type Out[N] = F[unapplyA.Out[N], unapplyB.Out[N]]
      def reapply[X1](in: F[A, B], x1: X1): F[unapplyA.Out[X1], unapplyB.Out[X1]] = applyF.apply(
        unapplyA.reapply(in.a, x1),
        unapplyB.reapply(in.b, x1)
      )
    }

  implicit def recurseCond[F[AA, BB] <: Comparison[F, AA, BB], A, B, X](implicit
    unapplyA: Unapply[A, X],
    unapplyB: Unapply[B, X],
    applyF: Apply2[F]
  ): Aux[F[A, B], X, ({type L[N] = F[unapplyA.Out[N], unapplyB.Out[N]]})#L] =
    new Unapply[F[A, B], X] {
      type Out[N] = F[unapplyA.Out[N], unapplyB.Out[N]]
      def reapply[X1](in: F[A, B], x1: X1): F[unapplyA.Out[X1], unapplyB.Out[X1]] = applyF.apply(
        unapplyA.reapply(in.a, x1),
        unapplyB.reapply(in.b, x1)
      )
    }
}

trait LowPriorityUnapply { self: Unapply.type =>
  implicit def const[V <: AnyVal, X]: Aux[V, X, ({type L[X1] = V})#L] = new Unapply[V, X] {
    type Out[X1] = V
    def reapply[X1](in: V, x1: X1): V = in
  }

  implicit def otherArg[A <: Int, B <: Int](implicit witness: Witness.Aux[A]): Aux[Arg[A], Arg[B], ({type L[X] = Arg[A]})#L] = new Unapply[Arg[A], Arg[B]] {
    type Out[X] = Arg[A]
    override def reapply[X1](in: Arg[A], x1: X1): Arg[A] = Arg(witness.value)
  }
}

trait Unapply2[Expr, A, B] {
  type Out[_, _]
  def reapply[X1, X2](expr: Expr, x1: X1, x2: X2): Out[X1, X2]
}

object Unapply2 {
  type Aux[Expr, A, B, Out0[_, _]] = Unapply2[Expr, A, B] { type Out[X1, X2] = Out0[X1, X2] }

  implicit def a[A, B]: Aux[A, A, B, ({type L[X1, X2] = X1})#L] = new Unapply2[A, A, B] {
    type Out[X1, X2] = X1
    def reapply[X1, X2](expr: A, x1: X1, x2: X2): X1 = x1
  }

  implicit def b[A, B]: Aux[B, A, B, ({type L[X1, X2] = X2})#L] = new Unapply2[B, A, B] {
    type Out[X1, X2] = X2
    def reapply[X1, X2](expr: B, x1: X1, x2: X2): X2 = x2
  }

  implicit def recurse1[F[AA] <: Expr1[F, AA], T, A, B](implicit
    unapplyT: Unapply2[T, A, B],
    applyF: Apply1[F]
  ): Aux[F[T], A, B, ({type L[X1, X2] = F[unapplyT.Out[X1, X2]]})#L] = new Unapply2[F[T], A, B] {
    type Out[X1, X2] = F[unapplyT.Out[X1, X2]]
    def reapply[X1, X2](expr: F[T], x1: X1, x2: X2): F[unapplyT.Out[X1, X2]] = applyF(unapplyT.reapply(expr.a, x1, x2))
  }

  implicit def recurse2[F[AA, BB] <: Expr2[F, AA, BB], T1, T2, A, B](implicit
    unapplyT1: Unapply2[T1, A, B],
    unapplyT2: Unapply2[T2, A, B],
    applyF: Apply2[F]
  ): Aux[F[T1, T2], A, B, ({type L[X1, X2] = F[unapplyT1.Out[X1, X2], unapplyT2.Out[X1, X2]]})#L] = new Unapply2[F[T1, T2], A, B] {
    type Out[X1, X2] = F[unapplyT1.Out[X1, X2], unapplyT2.Out[X1, X2]]
    def reapply[X1, X2](expr: F[T1, T2], x1: X1, x2: X2): F[unapplyT1.Out[X1, X2], unapplyT2.Out[X1, X2]] =
      applyF(unapplyT1.reapply(expr.a, x1, x2), unapplyT2.reapply(expr.b, x1, x2))
  }

  implicit def const[V <: AnyVal, A, B]: Aux[V, A, B, ({type L[X1, X2] = V})#L] = new Unapply2[V, A, B] {
    type Out[X1, X2] = V
    def reapply[X1, X2](expr: V, x1: X1, x2: X2): V = expr
  }
}

trait Apply1[F[_]] {
  def apply[A](a: A): F[A]
}

trait Apply2[F[_, _]] {
  def apply[A, B](a: A, b: B): F[A, B]
}