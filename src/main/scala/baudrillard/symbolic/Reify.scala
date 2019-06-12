package baudrillard.symbolic

import baudrillard.typeops.{Concat, ConcatS, ConstToString, ConstToStringP, FnCall}

trait Reify[Expr, T] {
  type Code <: String
}

object Reify extends LowPriorityReify {
  type Aux[Expr, T, Code0 <: String] = Reify[Expr, T] { type Code = Code0 }

  def apply[Expr, T](implicit inst: Reify[Expr, T]): Aux[Expr, T, inst.Code] = inst

  private[symbolic] val inst: Aux[Nothing, Nothing, Nothing] = new Reify[Nothing, Nothing] {
    type Code = Nothing
  }

  implicit def arg[I <: Int, T](implicit str: ConstToStringP[I, "x"]): Aux[Arg[I], T, str.Out] = inst.asInstanceOf[Aux[Arg[I], T, str.Out]]
  implicit def const[V <: AnyVal, T <: AnyVal : Numeric](implicit str: ConstToString[V]): Aux[V, T, str.Out] = inst.asInstanceOf[Aux[V, T, str.Out]]

  implicit def negLI[B, T <: AnyVal : Numeric, CodeB <: String](implicit
    b: Reify.Aux[B, T, CodeB],
    concat: Concat[("-(", CodeB, ")")]
  ): Aux[*[-1, B], T, concat.Out] = inst.asInstanceOf[Aux[*[-1, B], T, concat.Out]]

  implicit def negRI[A, T <: AnyVal : Numeric, CodeA <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    concat: Concat[("-(", CodeA, ")")]
  ): Aux[*[A, -1], T, concat.Out] = inst.asInstanceOf[Aux[*[A, -1], T, concat.Out]]

  implicit def negLD[B, T <: AnyVal : Numeric, CodeB <: String](implicit
    b: Reify.Aux[B, T, CodeB],
    concat: Concat[("-(", CodeB, ")")]
  ): Aux[*[-1.0, B], T, concat.Out] = inst.asInstanceOf[Aux[*[-1.0, B], T, concat.Out]]

  implicit def negRD[A, T <: AnyVal : Numeric, CodeA <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    concat: Concat[("-(", CodeA, ")")]
  ): Aux[*[A, -1.0], T, concat.Out] = inst.asInstanceOf[Aux[*[A, -1.0], T, concat.Out]]

  implicit def addPrimitive[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concatS: ConcatS[(CodeA, CodeB), " + "]
  ): Aux[A + B, T, concatS.Out] = inst.asInstanceOf[Aux[A + B, T, concatS.Out]]

  implicit def powDouble[A, P, CodeA <: String, CodeP <: String](implicit
    a: Reify.Aux[A, Double, CodeA],
    p: Reify.Aux[P, Double, CodeP],
    call: FnCall["java.lang.Math.pow", (CodeA, CodeP)]
  ): Aux[A^P, Double, call.Out] = inst.asInstanceOf[Aux[A^P, Double, call.Out]]

  implicit def expDouble[A, CodeA <: String](implicit
    a: Reify.Aux[A, Double, CodeA],
    call: FnCall["java.lang.Math.exp", CodeA]
  ): Aux[Exp[A], Double, call.Out] = inst.asInstanceOf[Aux[Exp[A], Double, call.Out]]

  implicit def logDouble[A, CodeA <: String](implicit
    a: Reify.Aux[A, Double, CodeA],
    call: FnCall["java.lang.Math.log", CodeA]
  ): Aux[Log[A], Double, call.Out] = inst.asInstanceOf[Aux[Log[A], Double, call.Out]]

  implicit def sinDouble[A, CodeA <: String](implicit
    a: Reify.Aux[A, Double, CodeA],
    call: FnCall["java.lang.Math.sin", CodeA]
  ): Aux[Sin[A], Double, call.Out] = inst.asInstanceOf[Aux[Sin[A], Double, call.Out]]

  implicit def cosDouble[A, CodeA <: String](implicit
    a: Reify.Aux[A, Double, CodeA],
    call: FnCall["java.lang.Math.cos", CodeA]
  ): Aux[Cos[A], Double, call.Out] = inst.asInstanceOf[Aux[Cos[A], Double, call.Out]]

  implicit def branch[CondExpr, Y, N, T, CodeC <: String, CodeY <: String, CodeN <: String](implicit
    cond: ReifyCond.Aux[CondExpr, CodeC],
    y: Aux[Y, T, CodeY],
    n: Aux[N, T, CodeN],
    concat: Concat[("if (", CodeC, ") { ", CodeY, " } else { ", CodeN, " }")]
  ): Aux[Branch[CondExpr, Y, N], T, concat.Out] = inst.asInstanceOf[Aux[Branch[CondExpr, Y, N], T, concat.Out]]
}

trait LowPriorityReify { self: Reify.type =>

  implicit def multiplyPrimitive[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concatS: ConcatS[(CodeA, CodeB), " * "]
  ): Aux[*[A, B], T, concatS.Out] = inst.asInstanceOf[Aux[*[A, B], T, concatS.Out]]
}


trait ReifyCond[Expr] {
  type Code <: String
}

object ReifyCond {
  type Aux[Expr, Code0 <: String] = ReifyCond[Expr] { type Code = Code0 }

  private val inst: Aux[Nothing, Nothing] = new ReifyCond[Nothing] {
    type Code = Nothing
  }

  implicit def lt[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concat: ConcatS[(CodeA, CodeB), " < "]
  ): Aux[A < B, concat.Out] = inst.asInstanceOf[Aux[A < B, concat.Out]]

  implicit def lte[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concat: ConcatS[(CodeA, CodeB), " <= "]
  ): Aux[A <= B, concat.Out] = inst.asInstanceOf[Aux[A <= B, concat.Out]]

  implicit def gt[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concat: ConcatS[(CodeA, CodeB), " > "]
  ): Aux[A > B, concat.Out] = inst.asInstanceOf[Aux[A > B, concat.Out]]

  implicit def gte[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concat: ConcatS[(CodeA, CodeB), " >= "]
  ): Aux[A >= B, concat.Out] = inst.asInstanceOf[Aux[A >= B, concat.Out]]

  implicit def eq[A, B, T <: AnyVal : Numeric, CodeA <: String, CodeB <: String](implicit
    a: Reify.Aux[A, T, CodeA],
    b: Reify.Aux[B, T, CodeB],
    concat: ConcatS[(CodeA, CodeB), " == "]
  ): Aux[A == B, concat.Out] = inst.asInstanceOf[Aux[A == B, concat.Out]]

}