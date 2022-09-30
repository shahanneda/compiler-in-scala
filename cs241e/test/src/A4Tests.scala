import cs241e.assignments.A1.{loadAndRun, setMem}
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments._
import cs241e.mips.{State, Word}
import org.scalatest.FunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A4Tests extends FunSuite {
    lazy val varTestBasic = {
      val x = new Variable("x")
      val y = new Variable("y")
      val resultOfAdd = new Variable("resultOfAdd")

      val variables = Seq(
        x,
        y,
        resultOfAdd,
      )
      val endlabel = new Label("end")
      val oneIsSmaller = new Label("oneIsSmaller")

      val code = Scope(
        variables,
        block(
          LIS(Reg(10)),
          Word(encodeUnsigned(5)),
          LIS(Reg(11)),
          Word(encodeUnsigned(6)),
          LIS(Reg(12)),
          Word(encodeUnsigned(1)),

          write(x, Reg(10)),
          write(y, Reg(11)),
          binOp(read(Reg.result, x), plus, read(Reg.result, y)),
          write(resultOfAdd, Reg.result),
          binOp(
            binOp(read(Reg.result, resultOfAdd), plus, read(Reg.result, y)),
            plus,
            ADD(Reg.result, Reg(12))
          )
        )
      )
      compilerA4(code)
    }

  test("expression test 1"){
    printState(A4.loadAndRun(varTestBasic, debug = true))
  }

  test("maximumUnsigned"){
//    assert(loadAndRun(maximumUnsigned,
//      Word(Assembler.encodeUnsigned(555)),
//      Word(Assembler.encodeUnsigned((pow(2, 32).intValue() - 1))),
//    ).reg(3) != Assembler.encodeUnsigned(555))

  }

  def printState(s: State): Unit = {
    for (i <- 1 to 31) {
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println("memory")

    println(s.reg(Reg.targetPC.number))
  }
}
