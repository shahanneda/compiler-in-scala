import cs241e.assignments.A5._
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders.{binOp, leCmp, ltCmp, plus, whileLoop}
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments.{ProgramRepresentation, _}
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
    val garg2 = new Variable("garg2")
    val farg1 = new Variable("farg1")
    val harg1 = new Variable("harg1")

    val hvar1 = new Variable("hvar1");
    val hvar2 = new Variable("hvar2");
    val hvar3 = new Variable("hvar3-counter");
    val h = new Procedure("h", Seq(harg1), Option(main))
    val f = new Procedure("f", Seq(farg1), Option(h))
    val g = new Procedure("g", Seq(garg1, garg2), Option(f))

    println("main outer is " + main.outer)
    val fvar = new Variable("fvar");
    val closeVar = new Variable("closure Variable");

    f.code = Scope(Seq(fvar),
      block(
        Comment("f body"),
        LIS(Reg.result),
        Word(encodeSigned(3)),
        //write(fvar, Reg.result),
//        read(Reg(13), arg1),
//        Call(g, Seq(block(
//          LIS(Reg.result),
//          Word(encodeSigned(4)),
//        ))),
        //Call(g, Seq(constRes(10))),
        //Closure(g),
        //write(fvar, Reg.result),
        //CallClosure(readVarRes(fvar), Seq(readVarRes(fvar)), Seq(new Variable("tmp-fvar"))),
        Closure(g),
      )
    )

    // reg 14 == 3
    // reg 16 ==  99
    var gvar1 = new Variable("gvar1");
    var mainVar = new Variable("main-var");

    g.code = Scope(Seq(gvar1), block(
      Comment("g body"),
      /*
      readVarRes(harg1),
      readVarRes(farg1),
      readVarRes(garg1),
      readVarRes(garg2),
      readVarRes(gvar1),
      readVarRes(fvar),
      readVarRes(hvar3),
      */
      //readVarRes(hvar2),
      //readVarRes(hvar3),
      /*
      Closure(g),
      write(gvar1, Reg.result),
      Comment("G loading variable from h"),
      Comment("Done loading"),
      */
      //CallClosure(readVarRes(garg1), Seq(readVarRes(gvar1)), Seq(new Variable("tmp-gvar"))),
    ))

    h.code = {
      //binOp(binOp(read(Reg.result, harg1), plus, read(Reg.result, farg1)), plus, readVarRes(farg1))
      Scope(Seq(hvar1, hvar2, hvar3), block(
        Closure(f),
        write(hvar1, Reg.result),
        constRes(99999),
        write(hvar3, Reg.result),
        Closure(g),
        write(hvar2, Reg.result),
        constRes(9999),
        write(harg1, Reg.result),
        Comment("Before calling closure f from h"),
        CallClosure(readVarRes(hvar1), Seq(readVarRes(hvar2)), Seq(new Variable("tmp")))
      ))
    }

    main.code = Scope(Seq(mainVar), block(
      //call(g, constRes(10), constRes(19)),
      call(h, constRes(10)),
      write(mainVar, Reg.result),
      Comment("Main var just wrote"),
      CallClosure(read(Reg.result, mainVar), Seq(constRes(5), constRes(10)),
       Seq(new Variable("c-1"), new Variable("c-2")),
      ),
      /*
       */
      //Closure(g),
      //CallClosure(ADD(Reg.result, Reg.result), Seq(ADD(Reg.result, Reg.result)), Seq(new Variable("tmp")))
      //call(h, constRes(10))
      //call(g, constRes(99))
    ))
    // 8 from 16777168

    val code = compilerA6(Seq(main, f, g, h))
    printState(A4.loadAndRun(code, Word(encodeUnsigned(1)), Word(encodeUnsigned(2)), debug = true))
//    printState(A4.loadAndRun(code, debug = false))
  }

  /*
  Accessing parent args and parent params (upto two nests)
  closure with 2 params
  closure with 1 params
  closure with no params

  // Closure accesses stack variables
   */
  test("increaseBy") {
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    val increaseByGeneratorArg = new Variable("increaseByGeneratorArg");
    val increaseByGeneratorVar = new Variable("ibgv ")
    val increaseByGenerator = new Procedure("increaseByGenerator", Seq(increaseByGeneratorArg), Option(main))

    val increaseArg = new Variable("increase arg")
    val increaseArg2 = new Variable("increase arg 2")
    val increase = new Procedure("increase procedure", Seq(increaseArg, increaseArg2), Option(increaseByGenerator))

    val nothingGenerator = new Procedure("nothingGen", Seq(), Option(main))
    val nothingProcedure = new Procedure("", Seq(), Option(nothingGenerator))

    nothingProcedure.code = block()
    nothingGenerator.code = block(
      Closure(nothingProcedure)
    )

    val doubleArg = new Variable("double var");
    val double = new Procedure("double", Seq(doubleArg), Option(main))
    double.code = {
      CallClosure(readVarRes(doubleArg),
        Seq(
          block(
            Comment("before inner call"),
            CallClosure(readVarRes(doubleArg),
                Seq(constRes(10), constRes(20)),
                Seq(new Variable("tmp0-0"), new Variable("tmp1-0"))
            ),
            Comment("after inner call"),
          ),
          constRes(10)
        ),
        Seq(new Variable("tmp0-1"), new Variable("tmp1-1"))
      )
    }


    increase.code = block(
      Comment("In increase"),
      binOp(binOp(read(Reg.result, increaseArg), plus, read(Reg.result, increaseByGeneratorArg)), plus, readVarRes(increaseArg2))
    )

    increaseByGenerator.code = Scope(Seq(increaseByGeneratorVar), block(
        constRes(100),
        write(increaseByGeneratorVar, Reg.result),
        Closure(increase),
      )
    )

    val in1 = new Variable("in1");
    val in2 = new Variable("in2");
    val in10 = new Variable("in10");

    // r15 = 11, r16 == 12, r17 == 20
    main.code = Scope(Seq(in1, in2, in10), block(
      Call(increaseByGenerator, Seq(constRes(1))),
      write(in1, Reg.result),
      /*
      Call(increaseByGenerator, Seq(constRes(2))),
      write(in2, Reg.result),
      Call(increaseByGenerator, Seq(constRes(10))),
      write(in10, Reg.result),
      CallClosure(read(Reg.result, in1), Seq(constRes(10), constRes(100)), Seq(new Variable("tmp"), new Variable("tmp"))),
      ADD(Reg(15), Reg.result),
      */
      /*
      CallClosure(read(Reg.result, in2), Seq(constRes(10), constRes(100)), Seq(new Variable("tmp") , new Variable("tmp"))),
      ADD(Reg(16), Reg.result),
      CallClosure(read(Reg.result, in10), Seq(constRes(10), constRes(100)), Seq(new Variable("tmp"), new Variable("tmp"))),
      ADD(Reg(17), Reg.result),
       */

      Closure(double),
      write(in2, Reg.result),
      CallClosure(readVarRes(in2), Seq(readVarRes(in1)), Seq(new Variable("tmp"))),
      ADD(Reg(15), Reg.result)
      //CallClosure(ADD(Reg.result, Reg.result), Seq(), Seq())
    ))

    //val code = compilerA6(Seq(main, increaseByGenerator, increase, nothingGenerator, nothingProcedure))
    val code = compilerA6(Seq(main, increaseByGenerator, increase, double))
    printState(A4.loadAndRun(code, Word(encodeUnsigned(1)), Word(encodeUnsigned(2)), debug = true))
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
  test("nested") {
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")

    val main = new Procedure("main", Seq(arg1, arg2))
    val garg1 = new Variable("garg1")
    val f = new Procedure("f", Seq(), Option(main))
    val g = new Procedure("g", Seq(garg1), Option(f))
    println("main outer is " + main.outer)

    val fvar = new Variable("fvar");

    f.code = Scope(Seq(fvar),
      block(
        constRes(120),
        write(fvar, Reg.result ),
        Call(g, Seq(block(
          constRes(121),
        )))
      )
    )

    g.code = block(
      read(Reg(14), arg2),
      read(Reg(15), fvar),
      read(Reg(16), garg1)
    )

    main.code = block(
      Call(f, Seq()),
    )
    val code = compilerA6(Seq(main, f, g))
    val out = A4.loadAndRun(code, Word(encodeUnsigned(1)), Word(encodeUnsigned(119)), debug = false);
    assert(out.reg(14) == encodeSigned(119))
    assert(out.reg(15) == encodeSigned(120))
    assert(out.reg(16) == encodeSigned(121))
  }

  test("simple tail"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    val farg1 = new Variable("farg1");
    val farg2 = new Variable("farg1");
    val f = new Procedure("f", Seq(farg1, farg2));
    f.code = block(
      ifStmt(readVarRes(farg1), ltCmp, constRes(10000), block(
          read(Reg.result, farg1),
          Comment("IN F"),
          call(f, binOp(readVarRes(farg1), plus, constRes(1)), readVarRes(farg2))
      ), block(
        binOp(readVarRes(farg1), plus, readVarRes(farg2))
      ))
    )

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(0)),
      Call(f, Seq(ADD(Reg.result, Reg.scratch), constRes(500))) ,
      ADD(Reg.result, Reg.result),
    )

    val code = compilerA6(Seq(main, f))
    val out = A4.loadAndRun(code, debug = false);
    printState(out)
    assert(decodeUnsigned(out.reg(3)) == 10500)
  }

  test("simple tail2"){
    val arg1 = new Variable("arg1-main")
    val arg2 = new Variable("arg2-main")
    val main = new Procedure("main", Seq(arg1, arg2))

    val farg1 = new Variable("farg1");
    val farg2 = new Variable("farg1");
    val f = new Procedure("f", Seq(farg1, farg2));
    f.code = block(
      ifStmt(readVarRes(farg1), ltCmp, constRes(10000), block(
        read(Reg.result, farg1),
        Comment("IN F"),
        call(f, binOp(readVarRes(farg1), plus, constRes(1)), readVarRes(farg2))
      ), block(
        binOp(readVarRes(farg1), plus, readVarRes(farg2))
      ))
    )

    main.code = block(
      LIS(Reg.scratch),
      Word(encodeSigned(0)),
      Call(f, Seq(ADD(Reg.result, Reg.scratch), constRes(500))) ,
      ADD(Reg.result, Reg.result),
    )

    val code = compilerA6(Seq(main, f))
    val out = A4.loadAndRun(code, debug = false);
    printState(out)
    assert(decodeUnsigned(out.reg(3)) == 10500)
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
      Word(encodeSigned(929)),
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
