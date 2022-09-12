import cs241e.assignments.A1.{add134, followingAddress, loadAndRun, maximum, setMem}
import cs241e.assignments.{A1, Assembler, Debugger, Reg}
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

class A1Tests extends FunSuite {
  test("decodeUnsigned") {
    assert(
      Assembler.decodeUnsigned(Seq(false, false, true, false)) == 2
    )
    assert(
      Assembler.decodeUnsigned(Seq(true, false, false, true, false)) == 18
    )
  }

  test("decodeSigned") {
    assert(
      Assembler.decodeSigned(Seq(false, true, true, false)) == 6
    )

    assert(
      Assembler.decodeSigned(Seq(true, false, false, true, false)) == -14
    )
    assert(
      Assembler.decodeSigned(Seq(true)) == -1
    )
  }

  test("encodeUnsigned"){
    assert(
      Assembler.encodeUnsigned(5, 6) == Seq(false, false, false, true, false, true)
    )
    assert(
      Assembler.encodeUnsigned(10, 4) == Seq(true, false, true, false)
    )
  }

  test("encodeSigned"){
    assert(
      Assembler.encodeSigned(-1, 5) == Seq(true, true, true, true, true)
    )
    assert(
      Assembler.encodeSigned(-14, 5) == Seq(true, false, false, true, false)
    )
    assert(
      Assembler.encodeSigned(-6, 4) == Seq(true, false, true, false)
    )
  }
  test("maximum"){
       assert(loadAndRun(maximum, Word(Assembler.encodeUnsigned(99)), Word(Assembler.encodeUnsigned(555))).reg(3) == Assembler.encodeUnsigned(555))
       assert(loadAndRun(maximum, Word(Assembler.encodeUnsigned(99)), Word(Assembler.encodeUnsigned(1))).reg(3) == Assembler.encodeUnsigned(99))
  }
  test("followingAddress") {
    assert(loadAndRun(followingAddress, Word(Assembler.encodeUnsigned(100)), Word(Assembler.encodeUnsigned(555))).reg(3) == Assembler.encodeUnsigned(104))
  }

  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}
