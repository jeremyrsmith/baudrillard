package baudrillard.typeops

import scala.reflect.macros.whitebox

trait AddInt[A <: Int, B <: Int] {
  type Out <: Int
  def result: Out
}

object AddInt {
  type Aux[A <: Int, B <: Int, Out0 <: Int] = AddInt[A, B] { type Out = Out0 }
  implicit def materialize[A <: Int, B <: Int, C <: Int]: Aux[A, B, C] = macro IntMacros.add[A, B, C]
}


trait SubtractInt[A <: Int, B <: Int] {
  type Out <: Int
  def result: Out
}

object SubtractInt {
  type Aux[A <: Int, B <: Int, Out0 <: Int] = SubtractInt[A, B] { type Out = Out0 }
  implicit def materialize[A <: Int, B <: Int, C <: Int]: Aux[A, B, C] = macro IntMacros.subtract[A, B, C]
}


trait MultiplyInt[A <: Int, B <: Int] {
  type Out <: Int
  def result: Out
}

object MultiplyInt {
  type Aux[A <: Int, B <: Int, Out0 <: Int] = MultiplyInt[A, B] { type Out = Out0 }
  implicit def materialize[A <: Int, B <: Int, C <: Int]: Aux[A, B, C] = macro IntMacros.add[A, B, C]
}


class IntMacros(val c: whitebox.Context) {
  import c.universe._

  private def op[A <: Int : WeakTypeTag, B <: Int : WeakTypeTag](typC: Type, auxTypC: Type, op: (Int, Int) => Int): Tree = {
    val A = weakTypeOf[A].dealias
    val B = weakTypeOf[B].dealias

    val ConstantType(Constant(a: Int)) = A
    val ConstantType(Constant(b: Int)) = B
    val result = op(a, b)
    val C = c.internal.constantType(Constant(result))

    val typ = appliedType(typC.typeConstructor, A, B)
    val auxTyp = appliedType(auxTypC.typeConstructor, A, B, C)

    q"""
       new $typ {
         type Out = $C
         val result: Out = $result
       }: $auxTyp
    """
  }

  def add[A <: Int : WeakTypeTag, B <: Int : WeakTypeTag, C <: Int : WeakTypeTag]: Tree =
    op[A, B](weakTypeOf[AddInt[_, _]], weakTypeOf[AddInt.Aux[_, _, _]], _ + _)

  def subtract[A <: Int : WeakTypeTag, B <: Int : WeakTypeTag, C <: Int : WeakTypeTag]: Tree =
    op[A, B](weakTypeOf[SubtractInt[_, _]], weakTypeOf[SubtractInt.Aux[_, _, _]], _ - _)

  def multiply[A <: Int : WeakTypeTag, B <: Int : WeakTypeTag, C <: Int : WeakTypeTag]: Tree =
    op[A, B](weakTypeOf[MultiplyInt[_, _]], weakTypeOf[MultiplyInt.Aux[_, _, _]], _ * _)
}