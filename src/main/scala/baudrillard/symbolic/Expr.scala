package baudrillard.symbolic

import baudrillard.symbolic.calculus.Derivative
import shapeless.Witness

import scala.annotation.tailrec

trait Expr[A] { self: A =>

  def +[V <: AnyVal](v: V): A + v.type = new +(this, v)
  def +[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A + B = new +(this, b)
  def +(eObj: e.type): ExprPlusEBuilder[A] = ExprPlusEBuilder(this)

  def -[V <: AnyVal](v: V): A - v.type = new +(this, new *(v, -1))
  def -[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A - B = new +(this, new *(b, -1))

  def *[V <: AnyVal](v: V): A * v.type = new *(this, v)
  def *[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A * B = new *(this, b)

  def /[V <: AnyVal](v: V): A / v.type = new *(this, Inv(v))
  def /[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A / B = new *(this, Inv(b))

  def ^[I <: Int with Singleton](i: I): A ^ i.type = new ^(this, i)
  def ^[E <: Expr[E]](exponent: E)(implicit ev: E <:< Expr[E]): A ^ E = new ^(this, exponent)

  def inv: A ^ -1 = new ^(this, -1)

  def unary_- : *[-1, A] = new *(-1, this)

  def log: Log[A] = Log(this)

  def exp: Exp[A] = Exp(this)

  def sin: Sin[A] = Sin(this)

  def <[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A < B = new <(this, b)
  def <[B <: AnyVal](b: B): A < b.type = new <(this, b)

  def <=[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A <= B = new <=(this, b)
  def <=[B <: AnyVal](b: B): A <= b.type = new <=(this, b)

  def >[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A > B = new >(this, b)
  def >[B <: AnyVal](b: B): A > b.type = new >(this, b)

  def >=[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A >= B = new >=(this, b)
  def >=[B <: AnyVal](b: B): A >= b.type = new >=(this, b)

  def ==[B <: Expr[B]](b: B)(implicit ev: B <:< Expr[B]): A == B = new ==(this, b)
  def ==[B <: AnyVal](b: B): A == b.type = new ==(this, b)
}

trait VecExpr[A] { self: A =>

  // vector space operations
  def sum: Sum[A] = Sum(this)

  // inner product space operations
  def innerProduct[B](b: B): A ∙ B = new ∙(this, b)
  def dot[B](b: B): A ∙ B = innerProduct(b)
  def ∙[B](b: B): A ∙ B = innerProduct(b)
  def >*<[B](b: B): A ∙ B = innerProduct(b)

  // outer product space operations
  def outerProduct[B](b: B): A ⊗ B = new ⊗(this, b)
  def <*>[B](b: B): A ⊗ B = outerProduct(b)

  // element-wise operations
  def map[R](fn: Arg[0] => R)(implicit unapply: Unapply[R, Arg[0]]): VecMap[A, unapply.Out] =
    VecMap(this, unapply.reapply(fn(Arg[0](0)), LambdaArg[0](0)))

}

trait Args1[A] {
  def a: A
}

trait Expr1[F[_], A] extends Expr[F[A]] with Args1[A] { self: F[A] =>
  def a: A
}

trait Args2[A, B] extends Args1[A] {
  def b: B
}

trait Expr2[F[_, _], A, B] extends Expr1[({type L[AA] = F[AA, B]})#L, A] with Args2[A, B] { self: F[A, B] =>

}

final case class +[A, B](a: A, b: B) extends Expr2[+, A, B] {
  override def toString: String = s"($a + $b)"
}

object + {
  implicit object PlusApply extends Apply2[+] {
    def apply[A, B](a: A, b: B): A + B = new +(a, b)
  }
}

final case class *[A, B](a: A, b: B) extends Expr2[*, A, B] {
  override def toString: String = if (a == -1) {
    s"-$b"
  } else if (b == -1) {
    s"-$a"
  } else {
    s"($a * $b)"
  }
}

object * {
  implicit object MulApply extends Apply2[*] {
    def apply[A, B](a: A, b: B): A * B = new *(a, b)
  }
}

final case class Neg[A](a: A) extends Expr1[Neg, A] {
  override def toString: String = s"-$a"
}

object Neg {
  implicit object NegApply extends Apply1[Neg] {
    def apply[A](a: A): Neg[A] = Neg(a)
  }
}

final case class Inv[A](a: A) extends Expr1[Inv, A] {
  override def toString: String = s"$a⁻¹"
}

object Inv {
  implicit object InvApply extends Apply1[Inv] {
    def apply[A](a: A): Inv[A] = Inv(a)
  }
}

final case class ^[A, B](a: A, b: B) extends Expr2[^, A, B] {
  override def toString: String = b.toString match {
    case superscript(str) => s"$a$str"
    case str => s"$a^$str"
  }
}

object ^ {
  implicit object PowApply extends Apply2[^]  {
    def apply[A, B](a: A, b: B): A^B = new ^(a, b)
  }
}

final case class Log[A](a: A) extends Expr1[Log, A] {
  override def toString: String = s"log($a)"
}

object Log {
  implicit object LogApply extends Apply1[Log] {
    def apply[A](a: A): Log[A] = new Log(a)
  }

  implicit def derivative[X]: Derivative.Aux[Log[X], X, X ^ -1] = new Derivative[Log[X], X] {
    type Out = X ^ -1
    def expr(in: Log[X], x: X): X ^ -1 = ^(x, -1)
  }
}

final case class Exp[A](a: A) extends Expr1[Exp, A] {
  override def toString: String = a.toString match {
    case superscript(str) => s"e$str"
    case str => s"e^$str"
  }
}

object Exp {
  implicit object ExpApply extends Apply1[Exp] {
    def apply[A](a: A): Exp[A] = new Exp(a)
  }

  implicit def derivative[X]: Derivative.Aux[Exp[X], X, Exp[X]] = new Derivative[Exp[X], X] {
    type Out = Exp[X]
    def expr(in: Exp[X], x: X): Exp[X] = in
  }
}

final case class Sin[A](a: A) extends Expr1[Sin, A] {
  override def toString: String = s"sin($a)"
}

object Sin {
  implicit object SinApply extends Apply1[Sin] {
    def apply[A](a: A): Sin[A] = new Sin(a)
  }

  implicit def derivative[X]: Derivative.Aux[Sin[X], X, Cos[X]] = new Derivative[Sin[X], X] {
    type Out = Cos[X]
    def expr(in: Sin[X], x: X): Cos[X] = Cos(x)
  }
}

final case class Cos[A](a: A) extends Expr1[Cos, A] {
  override def toString: String = s"cos($a)"
}

object Cos {
  implicit object CosApply extends Apply1[Cos] {
    def apply[A](a: A): Cos[A] = new Cos[A](a)
  }

  implicit def derivative[X]: Derivative.Aux[Cos[X], X, Sin[X]] = new Derivative[Cos[X], X] {
    type Out = Sin[X]
    def expr(in: Cos[X], x: X): Sin[X] = Sin(x)
  }
}

final case class Sum[A](a: A) extends Expr1[Sum, A] {
  override def toString: String = s"sum($a)"
}

object Sum {
  implicit object SumApply extends Apply1[Sum] {
    def apply[A](a: A): Sum[A] = new Sum(a)
  }
}

final case class ∙[A, B](a: A, b: B) extends Expr2[∙, A, B] {
  override def toString: String = s"($a) ∙ ($b)"
}

object ∙ {
  implicit object InnerProductApply extends Apply2[∙] {
    def apply[A, B](a: A, b: B): A ∙ B = new ∙(a, b)
  }
}

final case class ⊗[A, B](a: A, b: B) extends Expr2[⊗, A, B] {
  override def toString: String = s"($a) ⊗ ($b)"
}

object ⊗ {
  implicit object OuterProductApply extends Apply2[⊗] {
    def apply[A, B](a: A, b: B): A ⊗ B = new ⊗(a, b)
  }
}

final case class VecMap[A, L[_]](a: A, l: L[LambdaArg[0]])


final case class Arg[N <: Int](n: N) extends Expr[Arg[N]] {
  override def toString: String = n match {
    case subscript(str) => s"x$str"
    case _ => super.toString
  }
}

final case class LambdaArg[N <: Int](n: N)
final case class VecArg[N <: Int](n: N) extends Expr[VecArg[N]] with VecExpr[VecArg[N]]


trait Cond[Self] { self: Self => }

trait Comparison[Self[_, _], A, B] extends Cond[Self[A, B]] { self: Self[A, B] =>
  val a: A
  val b: B
}

final case class <[A, B](a: A, b: B) extends Comparison[<, A, B] {
  override def toString: String = s"$a < $b"
}

object < {
  implicit object LessThanApply extends Apply2[<] {
    def apply[A, B](a: A, b: B): A < B = new <(a, b)
  }
}

final case class <=[A, B](a: A, b: B) extends Comparison[<=, A, B] {
  override def toString: String = s"$a <= $b"
}

object <= {
  implicit object LessOrEqualApply extends Apply2[<=] {
    override def apply[A, B](a: A, b: B): A <= B = new <=(a, b)
  }
}

final case class >[A, B](a: A, b: B) extends Comparison[>, A, B] {
  override def toString: String = s"$a > $b"
}

object > {
  implicit object GreaterThanApply extends Apply2[>] {
    override def apply[A, B](a: A, b: B): A > B = new >(a, b)
  }
}

final case class >=[A, B](a: A, b: B) extends Comparison[>=, A, B] {
  override def toString: String = s"$a >= $b"
}

object >= {
  implicit object GreaterOrEqualApply extends Apply2[>=] {
    override def apply[A, B](a: A, b: B): A >= B = new >=(a, b)
  }
}

final case class ==[A, B](a: A, b: B) extends Comparison[==, A, B] {
  override def toString: String = s"$a = $b"
}

object == {
  implicit object EqualToApply extends Apply2[==] {
    override def apply[A, B](a: A, b: B): A == B = new ==(a, b)
  }
}

final case class Branch[CondExpr, A, B](cond: CondExpr, a: A, b: B) extends Expr[Branch[CondExpr, A, B]] {
  override def toString: String = Branch.format(this)
}

object Branch {

  @tailrec final def unroll[CondExpr, A, B](accum: Seq[(Any, Any)], branch: Branch[CondExpr, A, B]): (Seq[(Any, Any)], Any) = {
    val nextAccum = accum :+ (branch.cond -> branch.a)
    branch.b match {
      case b @ Branch(_, _, _) => unroll(nextAccum, b)
      case b => nextAccum -> b
    }
  }

  private def format[CondExpr, A, B](branch: Branch[CondExpr, A, B]): String = {
    def clean(expr: Any) = expr.toString.stripPrefix("(").stripSuffix(")")

    val (cases, otherwise) = unroll(Seq.empty, branch)
    val exprs = cases.map {
      case (cond, expr) => (" | " + cond.toString) -> clean(expr)
    } :+ ("   otherwise" -> clean(otherwise))

    val len = exprs.map(_._2.length).max
    val aligned = exprs.map {
      case (cond, expr) => expr.padTo(len, ' ') + cond
    }

    formatting.addLeftBorder(aligned, formatting.BigLeftBrace)
  }

  implicit def unapplyBranch[Cond, A, B, X](implicit
    unapplyC: Unapply[Cond, X],
    unapplyA: Unapply[A, X],
    unapplyB: Unapply[B, X]
  ): Unapply.Aux[Branch[Cond, A, B], X, ({type L[X1] = Branch[unapplyC.Out[X1], unapplyA.Out[X1], unapplyB.Out[X1]]})#L] = new Unapply[Branch[Cond, A, B], X] {
    type Out[X1] = Branch[unapplyC.Out[X1], unapplyA.Out[X1], unapplyB.Out[X1]]
    def reapply[X1](in: Branch[Cond, A, B], x1: X1): Branch[unapplyC.Out[X1], unapplyA.Out[X1], unapplyB.Out[X1]] =
      Branch(unapplyC.reapply(in.cond, x1), unapplyA.reapply(in.a, x1), unapplyB.reapply(in.b, x1))
  }

  implicit def simplify[Cond, A, B](implicit
    simplifyCond: Simplify[Cond],
    simplifyA: Simplify[A],
    simplifyB: Simplify[B]
  ): Simplify.Aux[Branch[Cond, A, B], Branch[simplifyCond.Out, simplifyA.Out, simplifyB.Out]] = new Simplify[Branch[Cond, A, B]] {
    type Out = Branch[simplifyCond.Out, simplifyA.Out, simplifyB.Out]
    override def apply(expr: Branch[Cond, A, B]): Branch[simplifyCond.Out, simplifyA.Out, simplifyB.Out] = Branch(simplifyCond(expr.cond), simplifyA(expr.a), simplifyB(expr.b))
  }

  implicit def derivative[Cond, A, B, X](implicit
    dA: Derivative[A, X],
    dB: Derivative[B, X]
  ): Derivative.Aux[Branch[Cond, A, B], X, Branch[Cond, dA.Out, dB.Out]] = new Derivative[Branch[Cond, A, B], X] {
    type Out = Branch[Cond, dA.Out, dB.Out]
    override def expr(f: Branch[Cond, A, B], x: X): Branch[Cond, dA.Out, dB.Out] = Branch(f.cond, dA.expr(f.a, x), dB.expr(f.b, x))
  }

}

private object formatting {
  case class Borders(normal: String, top: String, middle: String, bottom: String, specialCase2: Option[(String, String)])

  val BigLeftBrace = Borders("│", "╭", "┤", "╰", Some(("⎰", "⎱")))

  def addLeftBorder(lines: Seq[String], borders: Borders): String = borders match {
    case Borders(normalBorder, topBorder, middleBorder, bottomBorder, _) if lines.size > 2 =>
      val middleIndex = lines.size / 2
      val (beforeMiddle, middleAndAfter) = lines.splitAt(middleIndex)
      val middle +: afterMiddle = middleAndAfter
      topBorder + beforeMiddle.map(normalBorder + _).mkString + middleBorder + middle + afterMiddle.map(bottomBorder + _)

    case Borders(_, _, _, _, Some((top, bottom))) if lines.size == 2 =>
      val Seq(a, b) = lines
      top + a + bottom + b

    case Borders(_, top, _, bottom, None) if lines.size == 2 =>
      val Seq(a, b) = lines
      top + a + bottom + b

    case Borders(border, _, _, _, _) =>
      lines.map(border + _).mkString
  }

  def addLeftBorder(str: String, borders: Borders): String = addLeftBorder(str.linesWithSeparators.toSeq, borders)

}

private object superscript {
  val superscriptMap = Map(
    '(' -> "⁽",
    ')' -> "⁾",
    '+' -> "⁺",
    '*' -> " ⃰",
    '-' -> "⁻",
    '0' -> "⁰",
    '1' -> "¹",
    '2' -> "²",
    'x' -> "ˣ",
    'i' -> "ⁱ",
    's' -> "ˢ",
    'n' -> "ⁿ",
    'c' -> "ᶜ",
    'o' -> "ᵒ",
    'l' -> "ˡ"
  ) ++ (3 until 9).map(i => (0x30 + i).toChar -> new String(Character.toChars(0x2070 + i)))
  def unapply(str: String): Option[String] = if (str.forall(superscriptMap.contains)) Some(str.flatMap(superscriptMap)) else None

}

private object subscript {
  def unapply(i: Int): Option[String] = {
    val digitOffset = 0x2080 - 0x0030
    i match {
      case n if n < 0 => None
      case n => Some {
        n.toString.toSeq.flatMap {
          ch => Character.toChars(ch + digitOffset)
        }.mkString
      }
    }
  }
}

/*
trait Expr[A] { this: A =>

  def +[B <: AnyRef](b: B): A + B = new +(this, b)
  def +(b: Int): A + b.type = new +(this, b)
  def +(b: Double): A + b.type = new +(this, b)

  def *[B <: AnyRef](b: B): A * B = new *(this, b)
  def *(b: Int): A * b.type = new *(this, b)
  def *(b: Double): A * b.type = new *(this, b)

  def -[B <: AnyRef](b: B): A + -[B] = new +(this, new -(b))
  def -(b: Int): A + -[b.type] = new +(this, new -(b))
  def -(b: Double): A + -[b.type] = new +(this, new -(b))

  def /[B <: AnyRef](b: B): A * Inv[B] = new *(this, Inv(b))
  def /(b: Int): A * Inv[b.type] = new *(this, Inv(b))
  def /(b: Double): A * Inv[b.type] = new *(this, Inv(b))

  def unary_- : -[A] = new -(this)


}

final case class +[A, B](a: A, b: B) extends Expr[A + B]
final case class *[A, B](a: A, b: B) extends Expr[A * B]
final case class Inv[A](a: A) extends Expr[Inv[A]]
final case class -[A](a: A) extends Expr[-[A]]

final case class ^[A, B](a: A, b: B) extends Expr[A^B]

final case class Log[A](a: A) extends Expr[Log[A]]

final case class Exp[A](a: A) extends Expr[Exp[A]]

final case class Arg[I <: Int](i: I) extends Expr[Arg[I]]
 */