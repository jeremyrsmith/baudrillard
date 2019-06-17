package baudrillard.symbolic

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object SymbolicFunctionTests extends Properties("SymbolicFunction1") {

  // once we start overflowing the double range, things get weird... forgive me for ignoring that for now :)
  private val genReal = Gen.choose(-300.0, 300.0)

  val logistic = SymbolicFunction1 {
    x => 1 / (1 + e^(-x))
  }

  // compile-time assertion: type correct after application?
  val logisticApplied: (1 + Exp[-1 * Arg[0]]) ^ -1 = logistic(Arg[0](0))

  private val dLogistic = logistic.derivative

  private val logisticDouble = logistic.toDoubleOperator
  private val dLogisticDouble = dLogistic.toDoubleOperator
  println(dLogistic)

  private def scalaLogistic(x: Double): Double = math.pow(1 + math.exp(-x), -1)
  private def scalaDLogistic(x: Double): Double = math.exp(x) / math.pow(1 + math.exp(x), 2)

  property("logistic is correct") = forAll(genReal) { x =>
    math.abs(scalaLogistic(x) - logisticDouble.applyAsDouble(x)) < 1e-9
  }

  property("derivative of logistic is correct") = forAll(genReal) { x =>
    math.abs(scalaDLogistic(x) - dLogisticDouble.applyAsDouble(x)) < 1e-9
  }


}
