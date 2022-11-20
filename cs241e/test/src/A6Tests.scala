import cs241e.assignments.A5._
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders.{binOp, plus}
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

class A6Tests extends FunSuite {
  test("simple"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")

    val main = new Procedure("main", Seq(arg1, arg2))
    val garg1 = new Variable("garg1")
    val farg1 = new Variable("farg1")
    val f = new Procedure("f", Seq(), Option(main))
    val g = new Procedure("g", Seq(garg1), Option(f))
    val inner = new Procedure("inner", Seq(), Option(main))

    println("main outer is " + main.outer)

    val fvar = new Variable("fvar");
    val closeVar = new Variable("closure Variable");

    f.code = Scope(Seq(fvar),
      block(
        Comment("f body"),
        LIS(Reg.result),
        Word(encodeSigned(3)),
        write(fvar, Reg.result),
//        read(Reg(13), arg1),
//        Call(g, Seq(block(
//          LIS(Reg.result),
//          Word(encodeSigned(4)),
//        ))),
        Closure(g),
      )
    )

    // reg 14 == 3
    // reg 16 ==  99


    g.code = block(
      read(Reg(14), fvar),
      read(Reg(16), garg1)
    )

    var mainVar = new Variable("main-var");
    main.code = Scope(Seq(mainVar), block(
      call(f),
      write(mainVar, Reg.result),
      CallClosure(read(Reg.result, mainVar), Seq(constRes(99)),
        Seq(new Variable("c-1")),
      ),
      //call(g, constRes(99))
    ))
    // 8 from 16777168

    val code = compilerA6(Seq(main, f, g))
    printState(A4.loadAndRun(code, Word(encodeUnsigned(1)), Word(encodeUnsigned(2)), debug = true))
//    printState(A4.loadAndRun(code, debug = false))
  }

  test("increaseBy") {
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    val increaseByGeneratorArg = new Variable("ibg arg");
    val increaseByGenerator = new Procedure("increaseByGenerator", Seq(increaseByGeneratorArg))

    val increaseArg = new Variable("increase arg")
    val increase = new Procedure("", Seq(increaseArg), Option(increaseByGenerator))
    increase.code = block(
      binOp(read(Reg.result, increaseArg), plus, read(Reg.result, increaseByGeneratorArg))
    )

    increaseByGenerator.code = block(
      Closure(increase),
    )

    val in1 = new Variable("in1");
    val in2 = new Variable("in2");
    val in10 = new Variable("in10");

    // r15 = 11, r16 == 12, r17 == 20
    main.code = Scope(Seq(in1, in2, in10), block(
      Call(increaseByGenerator, Seq(constRes(1))),
      write(in1, Reg.result),
      Call(increaseByGenerator, Seq(constRes(2))),
      write(in2, Reg.result),
      Call(increaseByGenerator, Seq(constRes(10))),
      write(in10, Reg.result),

      CallClosure(read(Reg.result, in1), Seq(constRes(10)), Seq(new Variable("tmp") )),
      ADD(Reg(15), Reg.result),
      CallClosure(read(Reg.result, in2), Seq(constRes(10)), Seq(new Variable("tmp") )),
      ADD(Reg(16), Reg.result),
      CallClosure(read(Reg.result, in10), Seq(constRes(10)), Seq(new Variable("tmp") )),
      ADD(Reg(17), Reg.result),
    ))

    val code = compilerA6(Seq(main, increaseByGenerator, increase))
    printState(A4.loadAndRun(code, Word(encodeUnsigned(1)), Word(encodeUnsigned(2)), debug = false))
  }

  test("loop procedure"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(0)),
      Call(loop, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA6(Seq(main, loop))
    printState(A4.loadAndRun(code, debug = false))
  }

  test("printProcedure"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(9349)),
      Call(printProcedure, Seq(ADD(Reg.result, Reg.scratch))) ,
    )

    val code = compilerA6(Seq(main, printProcedure))
    printState(A4.loadAndRun(code, debug = false))
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

      val code = compilerA6(Seq(main, printArray, printProcedure))
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

      val code = compilerA6(Seq(main, printArray, printProcedure))
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

      val code = compilerA6(Seq(main) ++ treeHeight)
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
