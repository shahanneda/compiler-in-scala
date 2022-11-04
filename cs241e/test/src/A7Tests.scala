import cs241e.assignments.A1.{followingAddress, loadAndRun, maximum}
import cs241e.assignments.A7.{abcSubstring, decimalNumber}
import cs241e.assignments.{Assembler, Lacs, Reg, Scanning}
import cs241e.mips.{State, Word}
import cs241e.scanparse.DFAs.DFA
import org.scalatest.FunSuite

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A7Tests extends FunSuite {
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
    val out = Lacs.scan( ">= <= != == => = > < () {}")
    println(out)
  }




  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}
