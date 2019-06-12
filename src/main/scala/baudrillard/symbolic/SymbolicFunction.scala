package baudrillard.symbolic

import java.util.function.DoubleUnaryOperator

import baudrillard.symbolic.calculus.Derivative

abstract class SymbolicFunction1[Expr[_]](val expr: Expr[Arg[0]]) {
  type Applied = Expr[Arg[0]]

  def apply[A](a: A): Expr[A]

  def simplify[S, SS](implicit
    simplify: Simplify.Aux[Expr[Arg[0]], S],
    simplifyAgain: Simplify.Aux[S, SS],
    unapply: Unapply[SS, Arg[0]]
  ): SymbolicFunction1[unapply.Out] = SymbolicFunction1(arg => simplifyAgain(simplify(expr)))

  def derivative[D, DS, DSS](implicit
    derivative: Derivative.Aux[Expr[Arg[0]], Arg[0], D],
    simplify: Simplify.Aux[D, DS],
    simplifyAgain: Simplify.Aux[DS, DSS],
    unapply: Unapply[DSS, Arg[0]]
  ): SymbolicFunction1[unapply.Out] = SymbolicFunction1(arg => simplifyAgain(simplify(derivative.expr(expr, Arg(0)))))

  def reify[T](implicit reifyFunction: ReifyFunction1[Expr[Arg[0]], T]): T => T = reifyFunction
  def toDoubleOperator(implicit reifyDoubleOperator: ReifyDoubleOperator[Expr[Arg[0]]]): DoubleUnaryOperator = reifyDoubleOperator

}

object SymbolicFunction1 {

  def apply[R](fn: Arg[0] => R)(implicit
    unapply: Unapply[R, Arg[0]]
  ): SymbolicFunction1[unapply.Out] = {
    val R = fn(Arg[0](0))
    new SymbolicFunction1[unapply.Out](unapply.reapply(R, Arg[0](0))) {
      override def toString: String = s"x => ${unapply.reapply(R, "x")}"
      def apply[A](a: A): unapply.Out[A] = unapply.reapply(R, a)
    }
  }

}

abstract class SymbolicFunction2[Expr[_, _]](val expr: Expr[Arg[0], Arg[1]]) {
  def apply[A, B](a: A, b: B): Expr[A, B]

  def simplify[S](implicit
    simplify: Simplify.Aux[Expr[Arg[0], Arg[1]], S],
    unapply: Unapply2[S, Arg[0], Arg[1]]
  ): SymbolicFunction2[unapply.Out] = SymbolicFunction2((x1, x2) => simplify(expr))

  //def derivative

}

object SymbolicFunction2 {
  def apply[R](fn: (Arg[0], Arg[1]) => R)(implicit
    unapply: Unapply2[R, Arg[0], Arg[1]]
  ): SymbolicFunction2[unapply.Out] = {
    val R = fn(Arg[0](0), Arg[1](1))
    new SymbolicFunction2[unapply.Out](unapply.reapply(R, Arg[0](0), Arg[1](1))) {
      def apply[A, B](a: A, b: B): unapply.Out[A, B] = unapply.reapply(R, a, b)
    }
  }
}