import cs241e.assignments.Parsing.parseEarley
import cs241e.assignments.{Assembler, Lacs, Reg, Scanning, Typer}
import cs241e.mips.State
import cs241e.scanparse.DFAs.{DFA, Token}
import cs241e.scanparse.Grammars.{Tree, parseGrammar}
import org.scalatest.FunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A9Tests extends FunSuite {
  test("maximalMunchScan") {
    val dfa = DFA(
      alphabet = "abc".toSet,
      states = Set("start", "a", "b", "c", "a2", "b2"),
      start = "start",
      accepting = Set("b", "c"),
      transition = {
        case ("start", 'a') => "a"
        case ("a", 'b') => "b"
        case ("b", 'c') => "c"
        case ("c", 'a') => "a2"
        case ("a2", 'b') => "b2"
      }
    )
    val out = Scanning.maximalMunchScan(dfa, "abcabcabcabcab")
    println(out)
  }
  test("lacsScanDfa") {
    val dfa = Lacs.dfa
//    val out = Lacs.scan( ">= <= != == => = > < () {} >+if ")
    //val out = Lacs.scan( " Int x = 45 / 0; <=//comment\n>=")
    val out = Lacs.scan( "hello09 INt" +
      " helloIf hello283  elseif def= " +
      "=> if def " +
      "var " +
      "Int " +
      "if " +
      "else int" +
      "  " +
      "0 " +
      "(    " +
      ") " +
      "{ " +
      "} " +
      "= " +
      "== " +
      "!= " +
      "< " +
      "> " +
      "<= " +
      ">= " +
      " + " +
      "- " +
      "* " +
      "/ " +
      "% " +
      ", " +
      "; : => //hello,.,.\n   ")
    println(out)
  }


  test("parse1"){
    val grammar = parseGrammar(
      """
S BOF A EOF
A x
A A B A
B
    """)

    //val input = IndexedSeq(Token("BOF"), Token("x"), Token("x"), Token("EOF"));
    val input = IndexedSeq("BOF", "x", "x", "x", "EOF");


    val out = parseEarley(grammar, input)
    //println("longest is: ", ParsingPrivate.longestPrefix(grammar, input));
    println(out)
  }

  test("TypeTest 1"){
    //val input = "def hello () : Int = {6}";
    //val input = "def hello () : Int = { 5 + 5 }";
    val input = "def hello (a : Int, b : Int) : Int = { " +
      //"var x : Int ;" +
      "var x : (Int, Int, Int) => Int ;" +
      "var n : () => Int ;" +
      "var o : (Int) => (Int, Int, Int) => Int ;" +
      //"var y : () => Int ;" +
      //"var y : (Int, Int) => (Int) => Int ;" +
      //"var normal : Int ;" +
      //"x;" +
      "o(0);"+
      "5" +
      //"y = x;" +
      //"x = x + 2" +
      "}"
    //println(input)
    val out = Lacs.scanAndParse(input);
    //val vardef = out.children(1).children(0).children(9)

    //val a = ProcedureScope(input)

    //println(vardef.children(0).lhs.lexeme)
    //println(Typer.parseVarDefs(vardef));
    println(Typer.typeTree(out))
    //println(Typer.parseType(new Tree(Token("type", "INT"))));

    //println(Typer.parseType(out));

    //println(out)
  }



  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}