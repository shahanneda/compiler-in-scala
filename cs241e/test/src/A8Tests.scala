import cs241e.assignments.Parsing.{parseCYK, parseEarley}
import cs241e.assignments.{A8, Assembler, Lacs, Reg, Scanning}
import cs241e.mips.State
import cs241e.nosource.ParsingPrivate
import cs241e.scanparse.DFAs.{DFA, Token}
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

class A8Tests extends FunSuite {
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
  test("parse2"){
      //val input = "def hello () : Int = {6}";
      val input = "def hello () : Int = { 5 + 5)";
      val out = Lacs.scanAndParse(input);
      println(out)
  }
  test("parse3"){
    val grammar = Lacs.grammar
    val input = "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defedfsopt expras RBRACE".split(" ").toSeq

    println(input)
    val out = parseEarley(grammar, input.toIndexedSeq)
    println("longest is: ", ParsingPrivate.longestPrefix(grammar, input));
    println(out)
  }


  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}
