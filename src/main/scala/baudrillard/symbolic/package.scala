package baudrillard

package object symbolic {

  type -[A, B] = A + *[B, -1]
  type /[A, B] = A * Inv[B]

  implicit class OneOps(val self: 1) {
    def /[A](that: Expr[A]): ^[A, -1] = that ^ -1
    def +[A](that: Expr[A]): 1 + A = new +(self, that.asInstanceOf[A])
    def +(that: e.type): OnePlusEBuilder.type = OnePlusEBuilder
  }

  implicit class IntOps[I <: Int with Singleton](self: I) {
    def +[A](that: Expr[A]): I + A = new +(self, that.asInstanceOf[A])
  }

  object e {
    def ^[A](that: Expr[A]): Exp[A] = that.exp
  }

  case class ExprPlusEBuilder[A](expr: Expr[A]) {
    def ^[B](that: Expr[B]): A + Exp[B] = expr + that.exp
    def ^-[B](that: Expr[B]): A + Exp[*[-1, B]] = expr + Exp(-that)
  }

  object OnePlusEBuilder {
    def ^[B](that: Expr[B]): 1 + Exp[B] = new +(1, that.exp)
    def ^-[B](that: Expr[B]): 1 + Exp[*[-1, B]] = new +(1, (-that).exp)
  }

  def when[CondExpr <: Cond[CondExpr], T](whenExpr: (CondExpr, T)): WhenBuilder[((CondExpr, T), Unit)] = WhenBuilder((whenExpr, ()))

  case class WhenBuilder[Branches](branches: Branches) {
    def when[CondExpr2, T2](whenExpr: (CondExpr2, T2)): WhenBuilder[((CondExpr2, T2), Branches)] =
      WhenBuilder((whenExpr, branches))

    def otherwise[F, RB](falseExpr: F)(implicit
      reverse: WhenBuilder.ReverseBranches.Aux[Branches, RB],
      toBranches: WhenBuilder.ToBranches[RB, F]
    ): toBranches.Out = toBranches(reverse(branches), falseExpr)
  }

  object WhenBuilder {
    trait ToBranches[Branches, Otherwise] {
      type Out
      def apply(branches: Branches, otherwise: Otherwise): Out
    }

    object ToBranches {
      type Aux[Branches, Otherwise, Out0] = ToBranches[Branches, Otherwise] { type Out = Out0 }

      implicit def last[CondExpr <: Cond[CondExpr], T, F]: Aux[((CondExpr, T), Unit), F, Branch[CondExpr, T, F]] = new ToBranches[((CondExpr, T), Unit), F] {
        type Out = Branch[CondExpr, T, F]
        def apply(branch: ((CondExpr, T), Unit), otherwise: F): Branch[CondExpr, T, F] = Branch(branch._1._1, branch._1._2, otherwise)
      }

      implicit def recurse[CondExpr <: Cond[CondExpr], T, Rest, Otherwise](implicit
        restToBranches: ToBranches[Rest, Otherwise]
      ): Aux[((CondExpr, T), Rest), Otherwise, Branch[CondExpr, T, restToBranches.Out]] = new ToBranches[((CondExpr, T), Rest), Otherwise] {
        type Out = Branch[CondExpr, T, restToBranches.Out]
        def apply(branch: ((CondExpr, T), Rest), otherwise: Otherwise): Branch[CondExpr, T, restToBranches.Out] =
          Branch(branch._1._1, branch._1._2, restToBranches(branch._2, otherwise))
      }
    }

    trait ReverseBranches[T] {
      type Out
      def apply(t: T): Out
    }

    // (a, (b, (c, d)))
    // (a, ((b, c), d))
    // ((a, (b, c)), d)

    object ReverseBranches {
      type Aux[T, Out0] = ReverseBranches[T] { type Out = Out0 }
      def apply[T](implicit inst: ReverseBranches[T]): Aux[T, inst.Out] = inst

      implicit def derive[L, Out0](implicit reverse0: Reverse0[Unit, L, Out0]): Aux[L, Out0] = new ReverseBranches[L] {
        type Out = Out0
        def apply(l: L): Out0 = reverse0((), l)
      }

      trait Reverse0[Acc, Rest, Out] {
        def apply(acc: Acc, rest: Rest): Out
      }

      object Reverse0 {
        implicit def empty[Out]: Reverse0[Out, Unit, Out] = {
          (acc: Out, rest: Unit) => acc
        }

        implicit def reverse[Acc, A, B, Out](implicit
          rt: Reverse0[(A, Acc), B, Out]
        ): Reverse0[Acc, (A, B), Out] = {
          (acc, rest) => rt((rest._1, acc), rest._2)
        }
      }
    }

  }

}
