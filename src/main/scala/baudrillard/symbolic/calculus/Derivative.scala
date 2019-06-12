package baudrillard.symbolic
package calculus

import baudrillard.typeops.SubtractInt

trait Derivative[F, X] {
  type Out
  def expr(f: F, x: X): Out
}

object Derivative extends LowPriorityDerivative {
  type Aux[F, X, Out0] = Derivative[F, X] { type Out = Out0 }

  def apply[Expr, X](implicit inst: Derivative[Expr, X]): Aux[Expr, X, inst.Out] = inst

  implicit def identity[I <: Int]: Aux[Arg[I], Arg[I], 1] = new Derivative[Arg[I], Arg[I]] {
    type Out = 1
    def expr(arg: Arg[I], x: Arg[I]): 1 = 1
  }

  implicit def intCoefficientR[X, I <: Int]: Aux[X * I, X, I] = new Derivative[X * I, X] {
    type Out = I
    def expr(in: *[X, I], x: X): I = in.b
  }

  implicit def intCoefficientL[X, I <: Int]: Aux[I * X, X, I] = new Derivative[I * X, X] {
    type Out = I
    def expr(in: *[I, X], x: X): I = in.a
  }

  implicit def sum[A, B, X](implicit
    dA: Derivative[A, X],
    dB: Derivative[B, X]
  ): Aux[A + B, X, dA.Out + dB.Out] = new Derivative[A + B, X] {
    type Out = dA.Out + dB.Out
    def expr(apb: A + B, x: X) = new +(dA.expr(apb.a, x), dB.expr(apb.b, x))
  }

  implicit def inv[X]: Aux[Inv[X], X, Neg[Inv[X^2]]] = new Derivative[Inv[X], X] {
    type Out = Neg[Inv[X^2]]
    def expr(inv: Inv[X], x: X) = Neg(Inv(new ^(inv.a, 2)))
  }

  implicit def pow[X, P <: Int](implicit sub: SubtractInt[P, 1]): Aux[X^P, X, P * (X^(sub.Out))] = new Derivative[X^P, X] {
    type Out = P * (X^(sub.Out))
    def expr(in: X^P, x: X): P * (X^(sub.Out)) = new *(in.b, new ^(in.a, sub.result))
  }

}

trait LowPriorityDerivative extends LowerPriorityDerivative { self: Derivative.type =>

  implicit def product[A, B, X](implicit
    dA: Derivative[A, X],
    dB: Derivative[B, X]
  ): Aux[A * B, X, *[dA.Out, B] + *[A, dB.Out]] = new Derivative[A * B, X] {
    type Out = *[dA.Out, B] + *[A, dB.Out]
    def expr(atb: *[A, B], x: X) = new +(new *(dA.expr(atb.a, x), atb.b), new *(atb.a, dB.expr(atb.b, x)))
  }

  implicit def constInt[I <: Int, X]: Aux[I, X, 0] = new Derivative[I, X] {
    type Out = 0
    def expr(i: I, x: X): 0 = 0
  }

  implicit def constDouble[D <: Double, X]: Aux[D, X, 0] = new Derivative[D, X] {
    type Out = 0
    def expr(d: D, x: X): 0 = 0
  }

  implicit def chainPow[A, P <: Int, X](implicit
    unapplyA: Unapply[A, X],
    dA: Derivative[A, X],
    sub: SubtractInt[P, 1]
  ): Aux[A^P, X, *[*[P, ^[unapplyA.Out[X], sub.Out]], dA.Out]] = new Derivative[A^P, X] {
    type Out = *[*[P, ^[unapplyA.Out[X], sub.Out]], dA.Out]
    def expr(ap: A^P, x: X): *[*[P, ^[unapplyA.Out[X], sub.Out]], dA.Out] = new *(new *(ap.b, new ^(unapplyA.reapply(ap.a, x), sub.result)), dA.expr(ap.a, x))
  }

}

trait LowerPriorityDerivative { self: Derivative.type =>
  implicit def chain[F[_], GX, G[_], X, DFGX, DF[_], DGX](implicit
    unapplyG: Unapply.Aux[GX, X, G],
    dF: Derivative.Aux[F[GX], GX, DFGX],
    dG: Derivative.Aux[G[X], X, DGX],
    unapplyDF: Unapply.Aux[DFGX, GX, DF],
    applyF: Apply1[F],
    ev: F[GX] <:< Expr1[F, GX],
    ev2: GX <:< G[X]
  ): Aux[F[GX], X, DF[G[X]] * DGX] = {
    new Derivative[F[GX], X] {
      type Out = DF[G[X]] * DGX
      def expr(gfa: F[GX], x: X) = new *(unapplyDF.reapply(dF.expr(gfa, gfa.a), unapplyG.reapply(gfa.a, x)), dG.expr(ev2(gfa.a), x))
    }
  }
}