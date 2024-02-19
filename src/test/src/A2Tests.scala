import cs241e.assignments.A1.loadAndRun
import cs241e.assignments.A2.{maximum, maximumUnsigned}
import cs241e.assignments.{Assembler, Reg}
import cs241e.mips.{State, Word}
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

class A2Tests extends FunSuite {

  test("maximum"){
//    printState(loadAndRun(maximum, Word(Assembler.encodeUnsigned(99)), Word(Assembler.encodeUnsigned(555))))

       assert(loadAndRun(maximum, Word(Assembler.encodeSigned(-99)), Word(Assembler.encodeSigned(234))).reg(3) == Assembler.encodeSigned(234))
       assert(loadAndRun(maximum, Word(Assembler.encodeSigned(-100)), Word(Assembler.encodeSigned(-50))).reg(3) == Assembler.encodeSigned(-50))

  }

  test("maximumUnsigned"){
    assert(loadAndRun(maximumUnsigned, Word(Assembler.encodeUnsigned((pow(2, 32).intValue() - 1))), Word(Assembler.encodeUnsigned(555))).reg(3) != Assembler.encodeUnsigned(555))
    assert(loadAndRun(maximumUnsigned,
      Word(Assembler.encodeUnsigned(555)),
      Word(Assembler.encodeUnsigned((pow(2, 32).intValue() - 1))),
    ).reg(3) != Assembler.encodeUnsigned(555))

  }

  def printState(s: State): Unit = {
    for (i <- 1 to 31) {
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}
