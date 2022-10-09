import cs241e.assignments.A5._
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

class A5Tests extends FunSuite {
  test("loop procedure"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(0)),
      Call(loop, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA5(Seq(main, loop))
    printState(A4.loadAndRun(code, debug = true))
  }

  test("printProcedure"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(-239839349)),
      Call(printProcedure, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA5(Seq(main, printProcedure))
    printState(A4.loadAndRun(code, debug = false))
  }

  test("lastElement"){
    val state = A4.loadAndRunArray(A4.lastElement,
      Seq(Word(encodeUnsigned(0)), Word(encodeUnsigned(1)), Word(encodeUnsigned(2)), Word(encodeUnsigned(3)), Word(encodeUnsigned(99)))
      , true )
    printState(state)
    assert(state.reg(3) == Word(encodeUnsigned(99)))
  }

  test("lastElementEmpty"){
    val state = A4.loadAndRunArray(A4.lastElement,
      Seq()
      , true )
//    printState(state)
    assert(state.reg(3) == Word(encodeSigned(-1)))
  }
  test("maximumArray1"){
    val array = Seq(1, 2, 99, 2, 4, 9).map(x => Word(encodeUnsigned(x)))
    val state = A4.loadAndRunArray(A4.arrayMaximum, array, true )
    printState(state)
    assert(state.reg(3) == Word(encodeUnsigned(99)))
  }
  test("maximumArray2"){
    val array = Seq(-3, 2, 99, 2, 4, -34).map(x => Word(encodeSigned(x)))
    val state = A4.loadAndRunArray(A4.arrayMaximum, array, true )
    printState(state)
    assert(state.reg(3) == Word(encodeUnsigned(99)))
  }

  test("output letters"){
    val array = Seq(0, 0, 1, 2, 3, 0,0, 1,2,3, 26, 26).map(x => Word(encodeSigned(x)))
    val state = A4.loadAndRunArray(A4.outputLetters, array, false )
//    printState(state)
  }

  test("print Integer Code 1"){
//    println(encodeSigned(-2147483648))
    val state = A4.loadAndRun(A4.printInteger, Word(encodeSigned(-2147483648)), debug=false)
//    val state = A4.loadAndRun(A4.printInteger, Word(encodeSigned(0)), debug=false)
//    printState(state)
  }
  def printState(s: State): Unit = {
    for (i <- 1 to 31) {
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }

  }
}
