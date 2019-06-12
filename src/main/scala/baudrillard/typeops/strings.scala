package baudrillard.typeops

import scala.reflect.macros.whitebox

trait Concat2[A <: String, B <: String] {
  type Out <: String
}

object Concat2 {
  type Aux[A <: String, B <: String, Out0 <: String] = Concat2[A, B] { type Out = Out0 }
  implicit def concat2[A <: String, B <: String, Out <: String]: Aux[A, B, Out] = macro StringMacros.concat2[A, B, Out]
}

trait Concat[T <: Product] {
  type Out <: String
}

object Concat {
  type Aux[T <: Product, Out0 <: String] = Concat[T] { type Out = Out0 }
  implicit def concat[T <: Product, Out <: String]: Aux[T, Out] = macro StringMacros.concat[T, Out]
}

trait ConcatS[T <: Product, Sep <: String] {
  type Out <: String
}

object ConcatS {
  type Aux[T <: Product, Sep <: String, Out0 <: String] = ConcatS[T, Sep] { type Out = Out0 }
  def apply[T <: Product, Sep <: String](implicit inst: ConcatS[T, Sep]): Aux[T, Sep, inst.Out] = inst
  implicit def concatS[T <: Product, Sep <: String, Out <: String]: Aux[T, Sep, Out] = macro StringMacros.concatS[T, Sep, Out]
}

trait ConstToString[X <: AnyVal] {
  type Out <: String
}

object ConstToString {
  type Aux[X <: AnyVal, Out0 <: String] = ConstToString[X] { type Out = Out0 }
  implicit def constToString[X <: AnyVal, Out <: String]: Aux[X, Out] = macro StringMacros.constToString[X, Out]
}

trait ConstToStringP[X <: AnyVal, Pre <: String] {
  type Out <: String
}

object ConstToStringP {
  type Aux[X <: AnyVal, Pre <: String, Out0 <: String] = ConstToStringP[X, Pre] { type Out = Out0 }
  implicit def constToStringP[X <: AnyVal, Pre <: String, Out <: String]: Aux[X, Pre, Out] = macro StringMacros.constToStringP[X, Pre, Out]
}

trait FnCall[Fn <: String, Args] {
  type Out <: String
}

object FnCall {
  type Aux[Fn <: String, Args, Out0 <: String] = FnCall[Fn, Args] { type Out = Out0 }
  implicit def fnCall[Fn <: String, Args, Out <: String]: Aux[Fn, Args, Out] = macro StringMacros.fnCall[Fn, Args, Out]
}


class StringMacros(val c: whitebox.Context) {
  import c.universe._

  def concat2[A <: String : WeakTypeTag, B <: String : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val A = weakTypeOf[A].dealias
    val B = weakTypeOf[B].dealias
    val ConstantType(Constant(a: String)) = A
    val ConstantType(Constant(b: String)) = B
    val Out = c.internal.constantType(Constant(a + b))

    val auxType = appliedType(weakTypeOf[Concat2.Aux[_, _, _]].typeConstructor, A, B, Out)
    q"null.asInstanceOf[$auxType]"
  }

  def concat[T <: Product : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val T = weakTypeOf[T].dealias
    val elements = tupleTypes(T)
    val strings = elements.map {
      case ConstantType(Constant(str: String)) => str
      case typ => c.abort(c.enclosingPosition, s"Not a constant string type: $typ")
    }
    val Out = c.internal.constantType(Constant(strings.mkString))
    val auxType = appliedType(weakTypeOf[Concat.Aux[_, _]].typeConstructor, T, Out)
    q"null.asInstanceOf[$auxType]"
  }

  def concatS[T <: Product : WeakTypeTag, Sep <: String : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val T = weakTypeOf[T].dealias
    val Sep = weakTypeOf[Sep].dealias
    val ConstantType(Constant(sep: String)) = Sep
    val elements = tupleTypes(T)
    val strings = elements.map {
        case ConstantType(Constant(str: String)) => str
        case typ => c.abort(c.enclosingPosition, s"Not a constant string type: $typ")
    }
    val Out = c.internal.constantType(Constant(strings.mkString(sep)))
    val auxType = appliedType(weakTypeOf[ConcatS.Aux[_, _, _]].typeConstructor, T, Sep, Out)
    q"null.asInstanceOf[$auxType]"
  }

  def constToString[X <: AnyVal : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val X = weakTypeOf[X].dealias
    val ConstantType(Constant(x)) = X
    val str = constStr(x)
    val Out = c.internal.constantType(Constant(str))
    val auxType = appliedType(weakTypeOf[ConstToString.Aux[_, _]].typeConstructor, X, Out)
    q"null.asInstanceOf[$auxType]"
  }

  def constToStringP[X <: AnyVal : WeakTypeTag, Pre <: String : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val X = weakTypeOf[X].dealias
    val Pre = weakTypeOf[Pre].dealias
    val ConstantType(Constant(x)) = X
    val ConstantType(Constant(pre: String)) = Pre
    val str = constStr(x)
    val Out = c.internal.constantType(Constant(pre + str))
    val auxType = appliedType(weakTypeOf[ConstToStringP.Aux[_, _, _]].typeConstructor, X, Pre, Out)
    q"null.asInstanceOf[$auxType]"
  }

  def fnCall[Fn <: String : WeakTypeTag, Args : WeakTypeTag, Out <: String : WeakTypeTag]: Tree = {
    val Fn = weakTypeOf[Fn].dealias
    val Args = weakTypeOf[Args].dealias
    val ConstantType(Constant(fn: String)) = Fn
    val args = Args match {
      case ConstantType(Constant(x)) => List(constStr(x))
      case typ if typ.typeSymbol.fullName startsWith "scala.Tuple" => tupleTypes(typ).map {
        case ConstantType(Constant(x)) => x
        case _ => c.abort(c.enclosingPosition, s"Not a constant string type: $typ")
      }.map(constStr)
    }
    val Out = c.internal.constantType(Constant(s"$fn(${args.mkString(", ")})"))
    val auxType = appliedType(weakTypeOf[FnCall.Aux[_, _, _]].typeConstructor, Fn, Args, Out)
    q"null.asInstanceOf[$auxType]"
  }



  private def constStr(x: Any) = x match {
    case d: Double if d.isNaN => "java.lang.Double.NaN"
    case d: Double if d.isPosInfinity => "java.lang.Double.POSITIVE_INFINITY"
    case d: Double if d.isNegInfinity => "java.lang.Double.NEGATIVE_INFINITY"
    case other => other.toString
  }

  private def tupleTypes(typ: Type): List[Type] = {
    val typeName = typ.typeSymbol.fullName
    if (!typeName.startsWith("scala.Tuple"))
      c.abort(c.enclosingPosition, s"Not a tuple type: $typ")
    else
      typ.typeArgs.map(_.dealias)
  }

}

