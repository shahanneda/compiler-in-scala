import cs241e.assignments.Assembler.{encodeSigned, encodeUnsigned}
import cs241e.assignments.Parsing.parseEarley
import cs241e.assignments.Transformations.compilerA6
import cs241e.assignments._
import cs241e.mips.{State, Word}
import cs241e.scanparse.DFAs.DFA
import cs241e.scanparse.Grammars.parseGrammar
import org.scalatest.FunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A10Tests extends FunSuite {
  test("TypeTest 1"){
    //val input = "def hello () : Int = {6}";
    //val input = "def hello () : Int = { 5 + 5 }";
    val input = "def main (a : Int, b : Int) : Int = { " +
      " var x : Int ;" +
      " var n : (Int) => Int;" +
       "def f(a: Int) : Int = { " +
         "a + 10" +
        "}" +
       "x = 10; " +
      //"5"
      //"var x : (Int, Int, Int) => Int ;" +
      "n = f;" +
      //"var o : (Int) => (Int, Int, Int) => Int ;" +
      //"def inner(a : Int) : Int = { 3} " +
      //"var y : () => Int ;" +
      //"var y : (Int, Int) => (Int) => Int ;" +
      //"var normal : Int ;" +
      //"x;" +
      //"o(0);"+
      //"if (6 == 50) {5 - 2} else {4}" +
      //"y = x;" +
      //"x = x + 2" +
        "n(10) + f(20)" +
      "}"

    //println(input)
    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    println(typed)
    println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i.head.parameters)
    println(i.head.code)
    val code = compilerA6(i)
    val codeOut = A4.loadAndRun(code, debug=false)
    printState(codeOut)
  }



  test("Simple if 1"){
    val input = s"""
      def main (a : Int, b : Int) : Int = {
        var x : Int;
        x = a;
        if(x > 5){
          x = 10
        } else {
          x = x
        };
        x
      }
    """

    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    println(typed)
    println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i.head.parameters)
    println(i.head.code)
    val code = compilerA6(i)
    val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug=false)
    assert(codeOut.reg(3) == encodeUnsigned(4))
    val codeOut2 = A4.loadAndRun(code, Word(encodeUnsigned(100)), Word(encodeUnsigned(2)), debug=false)
    assert(codeOut2.reg(3) == encodeUnsigned(10))
    printState(codeOut)
  }
   test("Increase By") {
     val input =
       s"""
      def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def increaseByGen(i: Int) : (Int) => Int = {
          def increase(a : Int) : Int = {
            a + i
          }
          increase
        }

        inc1 = increaseByGen(1);
        inc2 = increaseByGen(10);
        inc3 = increaseByGen(100);
        inc1(30) + inc2(5) + inc3(90)
      }
    """

     val out = Lacs.scanAndParse(input);
     val typed = Typer.typeTree(out)
     //println(typed.procedureScopes.head.parms)
     val i = CodeGenerator.generateProcedures(typed)
     println(i(1).outer)
     //println(i.head.parameters)
     //println(i.head.code)
     val code = compilerA6(i)
     val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug = false)
     printState(codeOut)
     assert(codeOut.reg(3) == encodeUnsigned(236))
   }
  test("Increase doulble") {
    val input =
      s"""
      def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def increaseByGen(i: Int) : (Int) => Int = {
          def increase(a : Int) : Int = {
            a + i
          }
          increase
        }

        def double(f: (Int) => Int ) : (Int) => Int = {
          def h(a: Int) : Int = {
            f(f(a))
          }
          h
        }

        inc1 = double(increaseByGen(1));
        inc1(30) + double(increaseByGen(1))(30)
      }
    """

    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    //println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i(1).outer)
    //println(i.head.parameters)
    //println(i.head.code)
    val code = compilerA6(i)
    //println(code)
    val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug = true)
    printState(codeOut)
    assert(codeOut.reg(3) == encodeUnsigned(64))
  }

 test("Increase double 2") {
    val input =
      s"""
      def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def increaseByGen(i: Int) : (Int) => Int = {
          def increase(a : Int) : Int = {
            a + i
          }
          increase
        }

        def double(f: (Int) => Int ) : (Int) => Int = {
          def h(a: Int) : Int = {
            f(f(a))
          }
          h
        }

        inc1 = double(increaseByGen(1));
        inc1(30)
      }
    """

    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    //println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i(1).outer)
    //println(i.head.parameters)
    //println(i.head.code)
    val code = compilerA6(i)
    val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug = false)
    printState(codeOut)
    assert(codeOut.reg(3) == encodeUnsigned(32))
  }

  test("Tail call 1") {
    val input =
      s"""
      def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def f(i: Int) : Int = {
          if(i > 100000){
            10000
          }
          else{
            f(i + 1)
          }
        }

        f(0)
      }
    """

    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    //println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i(1).outer)
    //println(i.head.parameters)
    //println(i.head.code)
    val code = compilerA6(i)
    val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug = false)
    assert(codeOut.reg(3) == encodeUnsigned(10000))
  }

  test("Tail call closure 1") {
    val input =
      s"""
      def main (a : Int, b : Int) : Int = {
        var inc1 : (Int) => Int;
        var inc2 : (Int) => Int;
        var inc3 : (Int) => Int;

        def h(x : Int) : Int  = {
          f(x + 1, h)
        }

        def f(i: Int, g : (Int) => Int ) : Int = {
          if(i > 100){
            10000
          }
          else{
            g(i)
          }
        }

        f(0, h)
      }
    """

    val out = Lacs.scanAndParse(input);
    val typed = Typer.typeTree(out)
    //println(typed.procedureScopes.head.parms)
    val i = CodeGenerator.generateProcedures(typed)
    println(i(2).name, i(2).parameters(0), i(2).parameters(0).isPointer, i(2).parameters(1), i(2).parameters(1).isPointer)
    println(i.head.parameters)
    //println(i.head.code)
    val code = compilerA6(i)
    val codeOut = A4.loadAndRun(code, Word(encodeUnsigned(4)), Word(encodeUnsigned(2)), debug = false)
    printState(codeOut)
    assert(codeOut.reg(3) == encodeUnsigned(10000))
  }

  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}