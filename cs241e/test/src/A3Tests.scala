import cs241e.assignments.A1.{loadAndRun, setMem}
import cs241e.mips.{State, Word}
import cs241e.assignments.Assembler._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments._
import cs241e.assignments.Transformations._
import cs241e.mips._
import org.scalatest.FunSuite

import scala.math.pow

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A3Tests extends FunSuite {
    lazy val varTestBasic = {
      val x = new Variable("x")
      val y = new Variable("y")

      val variables = Seq(
        x,
        y
      )
      val endlabel = new Label("end")
      val oneIsSmaller = new Label("oneIsSmaller")

      val code = block(
        LIS(Reg(1)),
        Word(encodeUnsigned(5)),
        LIS(Reg(2)),
        Word(encodeUnsigned(99)),
        write(x, Reg(1)),
        write(y, Reg(2)),

        read(Reg(15), x),
        read(Reg(16), y),

        read(Reg(19), x),
        write(y, Reg(19)),

        read(Reg(17), y),
      )
      compilerA3(code, variables)
    }
  test("varTestBasic"){
    val state = setMem(varTestBasic.words);
    Debugger.debug(state, varTestBasic.debugTable)

    printState(loadAndRun(varTestBasic.words))
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
