package baudrillard.symbolic

import java.util.function.DoubleUnaryOperator

import scala.reflect.macros.whitebox

trait ReifyFunction1[Expr, @specialized T] extends (T => T)

object ReifyFunction1 {
  implicit def materialize[Expr, @specialized T, Code <: String](implicit reify: Reify.Aux[Expr, T, Code]): ReifyFunction1[Expr, T] = macro ReifyFunctionMacros.function1[Expr, T, Code]
}

trait ReifyDoubleOperator[Expr] extends DoubleUnaryOperator

object ReifyDoubleOperator {
  implicit def materialize[Expr, Code <: String](implicit reify: Reify.Aux[Expr, Double, Code]): ReifyDoubleOperator[Expr] = macro ReifyFunctionMacros.doubleOperator[Expr, Code]
}


class ReifyFunctionMacros(val c: whitebox.Context) {
  import c.universe._

  private def reify1[Expr : WeakTypeTag, T : WeakTypeTag, Code <: String : WeakTypeTag](methodName: String): Tree = {
    val Expr = weakTypeOf[Expr].dealias
    val T = weakTypeOf[T].dealias
    val Code = weakTypeOf[Code].dealias
    val ConstantType(Constant(code: String)) = Code
    val args = List("x0").map {
      argName => ValDef(Modifiers(), TermName(argName), TypeTree(T), EmptyTree)
    }

    val body = c.parse(s"($code)")
    val method = TermName(methodName)
    val resultTyp = c.macroApplication.tpe
    val result = q"""
       new $resultTyp {
         final def $method(..$args): $T = $body
       }
     """

    try c.typecheck(result) catch {
      case err: Throwable =>
        val e = err
        err.printStackTrace()
        throw e
    }
  }

  def function1[Expr : WeakTypeTag, T : WeakTypeTag, Code <: String : WeakTypeTag](reify: Tree): Tree =
    reify1[Expr, T, Code]("apply")

  def doubleOperator[Expr : WeakTypeTag, Code <: String : WeakTypeTag](reify: Tree): Tree =
    reify1[Expr, Double, Code]("applyAsDouble")
}