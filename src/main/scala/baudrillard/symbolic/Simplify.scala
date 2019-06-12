package baudrillard.symbolic

import baudrillard.typeops.{AddInt, MultiplyInt}
import shapeless.{Lazy, LowPriority}

trait Simplify[Expr] {
  type Out
  def apply(expr: Expr): Out
}

object Simplify extends Priority1Simplify {

  type Aux[Expr, Out0] = Simplify[Expr] { type Out = Out0 }

  def apply[Expr](implicit inst: Simplify[Expr]): Aux[Expr, inst.Out] = inst

  implicit def arg[N <: Int]: Aux[Arg[N], Arg[N]] = new Simplify[Arg[N]] {
    type Out = Arg[N]
    def apply(expr: Arg[N]): Arg[N] = expr
  }

  implicit def additiveIdentityR[Expr](implicit simplify: Simplify[Expr]): Aux[Expr + 0, simplify.Out] = new Simplify[Expr + 0] {
    type Out = simplify.Out
    def apply(expr: Expr + 0): simplify.Out = simplify(expr.a)
  }

  implicit def additiveIdentityL[Expr](implicit simplify: Simplify[Expr]): Aux[0 + Expr, simplify.Out] = new Simplify[0 + Expr] {
    type Out = simplify.Out
    def apply(expr: 0 + Expr): simplify.Out = simplify(expr.b)
  }

  implicit def multiplicativeIdentityR[Expr](implicit simplify: Simplify[Expr]): Aux[Expr * 1, simplify.Out] = new Simplify[Expr * 1] {
    type Out = simplify.Out
    def apply(expr: *[Expr, 1]):simplify.Out = simplify(expr.a)
  }

  implicit def multiplicativeIdentityL[Expr](implicit simplify: Simplify[Expr]): Aux[1 * Expr, simplify.Out] = new Simplify[1 * Expr] {
    type Out = simplify.Out
    def apply(expr: *[1, Expr]): simplify.Out = simplify(expr.b)
  }

  implicit def exponentIdentity[Expr](implicit simplify: Simplify[Expr]): Aux[Expr ^ 1, simplify.Out] = new Simplify[Expr ^ 1] {
    type Out = simplify.Out
    def apply(expr: Expr ^ 1): simplify.Out = simplify(expr.a)
  }

  implicit def combineCoefficients[E <: Expr[E], A <: Int, B <: Int](implicit addInt: AddInt[A, B], simplify: Simplify[E]): Aux[(*[A, E] + *[B, E]), *[addInt.Out, simplify.Out]] = new Simplify[*[A, E] + *[B, E]] {
    type Out = addInt.Out * simplify.Out
    def apply(expr: *[A, E] + *[B, E]): addInt.Out * simplify.Out = new *(addInt.result, simplify(expr.a.b))
  }

  implicit def negNegLL[E](implicit simplify: Simplify[E]): Aux[*[-1, *[-1, E]], simplify.Out] = new Simplify[*[-1, *[-1, E]]] {
    type Out = simplify.Out
    def apply(expr: *[-1, *[-1, E]]): simplify.Out = simplify(expr.b.b)
  }

  implicit def negNegLR[E](implicit simplify: Simplify[E]): Aux[*[-1, *[E, -1]], simplify.Out] = new Simplify[*[-1, *[E, -1]]] {
    type Out = simplify.Out
    def apply(expr: *[-1, *[E, -1]]): simplify.Out = simplify(expr.b.a)
  }

  implicit def negNegRL[E](implicit simplify: Simplify[E]): Aux[*[*[-1, E], -1], simplify.Out] = new Simplify[*[*[-1, E], -1]] {
    type Out = simplify.Out
    def apply(expr: *[*[-1, E], -1]): simplify.Out = simplify(expr.a.b)
  }

  implicit def negNegRR[E](implicit simplify: Simplify[E]): Aux[*[*[E, -1], -1], simplify.Out] = new Simplify[*[*[E, -1], -1]] {
    type Out = simplify.Out
    def apply(expr: *[*[E, -1], -1]): simplify.Out = simplify(expr.a.a)
  }

  implicit def negNegMulLL[A, B](implicit
    simplifyA: Simplify[A],
    simplifyB: Simplify[B]
  ): Aux[*[*[-1, A], *[-1, B]], simplifyA.Out * simplifyB.Out] = new Simplify[*[*[-1, A], *[-1, B]]] {
    type Out = simplifyA.Out * simplifyB.Out
    def apply(expr: *[*[-1, A], *[-1, B]]): simplifyA.Out * simplifyB.Out = new *(simplifyA(expr.a.b), simplifyB(expr.b.b))
  }

  implicit def combineExponentsMul[X, A <: Int, B <: Int](implicit addInt: AddInt[A, B], simplify: Simplify[X]): Aux[*[X^A, X^B], simplify.Out^addInt.Out] = new Simplify[*[X^A, X^B]] {
    type Out = simplify.Out^addInt.Out
    def apply(expr: *[X^A, X^B]): simplify.Out^addInt.Out = new ^(simplify(expr.a.a), addInt.result)
  }

  implicit def combineExponentsPow[X, A <: Int, B <: Int](implicit multiplyInt: MultiplyInt[A, B], simplify: Simplify[X]): Aux[(X^A)^B, simplify.Out ^ multiplyInt.Out] = new Simplify[(X^A)^B] {
    type Out = simplify.Out ^ multiplyInt.Out

    override def apply(expr: (X^A)^B): simplify.Out ^ multiplyInt.Out = new ^(simplify(expr.a.a), multiplyInt.result)
  }
}

trait Priority1Simplify extends Priority2Simplify { self: Simplify.type =>
  implicit def moveCoefficient[E <: Expr[E], N <: AnyVal](implicit simplify: Simplify[E]): Aux[E * N, N * simplify.Out] = new Simplify[E * N] {
    type Out = N * simplify.Out
    def apply(expr: *[E, N]): *[N, simplify.Out] = new *(expr.b, simplify(expr.a))
  }

  implicit def addConstantInt[A <: Int, B <: Int](implicit addInt: AddInt[A, B]): Aux[A + B, addInt.Out] = new Simplify[A + B] {
    type Out = addInt.Out
    def apply(expr: A + B): addInt.Out = addInt.result
  }

  implicit def multiplyConstantInt[A <: Int, B <: Int](implicit multiplyInt: MultiplyInt[A, B]): Aux[A * B, multiplyInt.Out] = new Simplify[*[A, B]] {
    type Out = multiplyInt.Out
    def apply(expr: *[A, B]): multiplyInt.Out = multiplyInt.result
  }
}

trait Priority2Simplify extends Priority3Simplify { self: Simplify.type =>

  implicit def recurse1Again[F[_], A, SA](implicit
    f: Apply1[F],
    simplifyA: Simplify.Aux[A, SA],
    simplifySA: Lazy[Simplify[SA]],
    ev: F[A] <:< Expr1[F, A]
  ): Aux[F[A], F[simplifySA.value.Out]] = new Simplify[F[A]] {
    type Out = F[simplifySA.value.Out]
    def apply(expr: F[A]): F[simplifySA.value.Out] = f(simplifySA.value(simplifyA(ev(expr).a)))
  }

  implicit def recurse2Again[F[_, _], A, B, SA, SB](implicit
    f: Apply2[F],
    simplifyA: Simplify.Aux[A, SA],
    simplifyB: Simplify.Aux[B, SB],
    simplifySA: Lazy[Simplify[SA]],
    simplifySB: Lazy[Simplify[SB]],
    ev: F[A, B] <:< Expr2[F, A, B]
  ): Aux[F[A, B], F[simplifySA.value.Out, simplifySB.value.Out]] = {
    new Simplify[F[A, B]] {
      type Out = F[simplifySA.value.Out, simplifySB.value.Out]
      override def apply(expr: F[A, B]): F[simplifySA.value.Out, simplifySB.value.Out] = f(simplifySA.value(simplifyA(ev(expr).a)), simplifySB.value(simplifyB(ev(expr).b)))
    }
  }

  implicit def recurseCond[F[_, _], A, B, SA, SB](implicit
    f: Apply2[F],
    simplifyA: Simplify.Aux[A, SA],
    simplifyB: Simplify.Aux[B, SB],
    simplifySA: Lazy[Simplify[SA]],
    simplifySB: Lazy[Simplify[SB]],
    ev: F[A, B] <:< Comparison[F, A, B]
  ): Aux[F[A, B], F[simplifySA.value.Out, simplifySB.value.Out]] = {
    new Simplify[F[A, B]] {
      type Out = F[simplifySA.value.Out, simplifySB.value.Out]
      override def apply(expr: F[A, B]): F[simplifySA.value.Out, simplifySB.value.Out] = f(simplifySA.value(simplifyA(ev(expr).a)), simplifySB.value(simplifyB(ev(expr).b)))
    }
  }

}

trait Priority3Simplify extends Priority4Simplify { self: Simplify.type =>

//  implicit def recurse1[F[_], A](implicit
//    f: Apply1[F],
//    simplifyA: Simplify[A],
//    ev: F[A] <:< Expr1[F, A]
//  ): Aux[F[A], F[simplifyA.Out]] = new Simplify[F[A]] {
//    type Out = F[simplifyA.Out]
//    def apply(expr: F[A]): F[simplifyA.Out] = f(simplifyA(ev(expr).a))
//  }
//
//  implicit def recurse2[F[_, _], A, B](implicit
//    f: Apply2[F],
//    simplifyA: Simplify[A],
//    simplifyB: Simplify[B],
//    ev: F[A, B] <:< Expr2[F, A, B]
//  ): Aux[F[A, B], F[simplifyA.Out, simplifyB.Out]] = new Simplify[F[A, B]] {
//    type Out = F[simplifyA.Out, simplifyB.Out]
//    override def apply(expr: F[A, B]): F[simplifyA.Out, simplifyB.Out] = f(simplifyA(ev(expr).a), simplifyB(ev(expr).b))
//  }

}

trait Priority4Simplify { self: Simplify.type =>

  implicit def default[Expr](implicit lowPriority: LowPriority): Aux[Expr, Expr] = new Simplify[Expr] {
    type Out = Expr
    def apply(expr: Expr): Expr = expr
  }

}