import cs241e.assignments.A5._
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments._
import cs241e.mips.{CPU, State, Word}
import org.scalatest.FunSuite

import java.io.{ByteArrayOutputStream, PrintStream}

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
    val a1 = new Variable("a1")
    val a2 = new Variable("a2")
    val main = new Procedure("main", Seq(a1, a2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(-2393349)),
      Call(printProcedure, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA5(Seq(main, printProcedure))
    A4.loadAndRun(code, debug = false)
  }

  test("twofunctiontest"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(500)),
      Call(twoProTest, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA5(Seq(main, twoProTest, procedure2, printProcedure))
    printState(A4.loadAndRun(code, debug = false))
  }


  test("printArray"){
    def test = () => {
      val arg1 = new Variable("arg1-main")
      val arg2 = new Variable("arg2-main")
      val main = new Procedure("main", Seq(arg1, arg2))

      val array = Seq(3, 49, 0, -234).map(x => Word(encodeSigned(x)))

      main.code = block(
        call(printArray, read(Reg.result, arg1), read(Reg.result, arg2))
      )

      val code = compilerA5(Seq(main, printArray, printProcedure))
      printState(A4.loadAndRunArray(code, array, debug = false))
    }
    testWithOutput(test, "3\n49\n0\n-234\n")
  }

  test("printArrayEmpty"){
    def test = () => {
      val arg1 = new Variable("arg1-main")
      val arg2 = new Variable("arg2-main")
      val main = new Procedure("main", Seq(arg1, arg2))

      val array = Seq().map(x => Word(encodeSigned(x)))

      main.code = block(
        call(printArray, read(Reg.result, arg1), read(Reg.result, arg2))
      )

      val code = compilerA5(Seq(main, printArray, printProcedure))
      printState(A4.loadAndRunArray(code, array, debug = false))
    }
    testWithOutput(test, "")
  }


  test("getTreeHeight"){
    def test = () => {
      val arg1 = new Variable("arg1-main")
      val arg2 = new Variable("arg2-main")
      val main = new Procedure("main", Seq(arg1, arg2))

//      val array = Seq(
//        77, 3, 6, 22, -1, -1, -8, 9, 12, -36, -1, -1, 999, -1, -1
//      ).map(x => Word(encodeSigned(x)))

      val array = Seq(
        77, 3, -1, 5, -1, -1
      ).map(x => Word(encodeSigned(x)))

      main.code = block(
        call(treeHeight(0), read(Reg.result, arg1), read(Reg.result, arg2))
      )

      val code = compilerA5(Seq(main) ++ treeHeight)
//      printState(A4.loadAndRunArray(code, array, debug = false))
      val a = A4.loadAndRunArray(code, array, debug = false)
      assert(a.reg(3) == encodeUnsigned(2))
    }
    test()
  }

  def printState(s: State): Unit = {
    for (i <- 1 to 31) {
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }

  }
  def testWithOutput(test: () => Unit, output: String): Unit ={
    // Obtain a Field handle and mark it accessible (ignoring the private modifier)
    val stream = CPU.getClass.getDeclaredField("outputStream")
    stream.setAccessible(true)
    // Save the old value of the field
    val prev = stream.get(CPU)
    // Create a new stream
    val buffer = new ByteArrayOutputStream

    try {
      // Set the field to our new stream
      stream.set(CPU, new PrintStream(buffer))

      test()

      // Retrieve the output using buffer.toString
      assert(buffer.toString == output)
    } finally {
      // Restore the old value of the field
      stream.set(CPU, prev)
    }
  }
}
