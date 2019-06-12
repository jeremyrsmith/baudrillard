package baudrillard.symbolic

import scala.reflect.runtime.universe.TypeTag

object ExprTest extends App {

  val a = Arg[0](0) + 2

  val fn = SymbolicFunction1 {
    a => a + 2
  }

//  val tt = implicitly[TypeTag[fn.Out[Arg[0]]]]
//  println(tt)


}
