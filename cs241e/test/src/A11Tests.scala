import cs241e.assignments.Assembler.{ADD, decodeSigned, decodeUnsigned, encodeUnsigned}
import cs241e.assignments.CodeBuilders.{binOp, divide}
import cs241e.assignments.MemoryManagement.{Chunk, GarbageCollector, heap, withGC}
import cs241e.assignments.ProgramRepresentation.{Code, Comment, Procedure, Scope, Variable, assign, block, call, read, write}
import cs241e.assignments.Transformations.compilerA6
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

class A11Tests extends FunSuite {
  test("Tail call closure 1") {
    val f = new Variable("f", isPointer = true)
    val chunk = Chunk(Seq(f))

    def newChunk = heap.allocate(chunk)

    def v(variable: Variable) = read(Reg.result, variable)

    def writeField(base: Code, field: Variable, value: Code): Code =
      binOp(base, chunk.store(Reg.scratch, field, Reg.result), value)

    val a = new Variable("a")
    val b = new Variable("b")
    val p = new Variable("p", isPointer = true)
    val q = new Variable("q", isPointer = true)
    val main = new Procedure("main", Seq(a, b))
    main.code = Scope(Seq(p, q), block(
      assign(p, newChunk),
      assign(q, newChunk),
      //writeField(v(q), f, v(p)),
      //assign(p, v(q)),
      call(GarbageCollector.collectGarbage),
      ADD(Reg.zero, Reg.zero)
    ))
    //val machineCode = compilerA6(main +: GarbageCollector.procedures)
    val machineCode = compilerA6(Seq(main))
    val finalState = A4.loadAndRun(machineCode, debug = true)
    println("reachable bytes: " + decodeSigned(finalState.reg(3)).toInt)

    def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
      if (words > 0) {
        println(spaces(address) + ": " + spaces(state.mem(address)))
        dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words - 1)
      }
    }

    def spaces(w: Word): String = w.toString.sliding(8, 8).mkString(" ")

    println("Semispace 1")
    dumpMem(finalState, GarbageCollector.heapStart)
    println("Semispace 2")
    dumpMem(finalState, GarbageCollector.heapMiddle)
    println("heapPtr: " + spaces(finalState.reg(Reg.heapPointer.number)))
  }

  test("heaps") {
    withGC {
      val f = new Variable("f", isPointer = true)
      val chunk = Chunk(Seq(f))

      def newChunk = heap.allocate(chunk)

      def v(variable: Variable) = read(Reg.result, variable)

      def writeField(base: Code, field: Variable, value: Code): Code =
        binOp(base, chunk.store(Reg.scratch, field, Reg.result), value)

      val a = new Variable("a")
      val b = new Variable("b")
      val p = new Variable("p", isPointer = true)
      val q = new Variable("q", isPointer = true)
      val chunk3loc = new Variable("chunk3loc", isPointer = true)

      //val q = new Variable("c", isPointer = true)

      val main = new Procedure("main", Seq(a, b))
      main.code = Scope(Seq(p, q, chunk3loc), block(
        assign(p, newChunk),
        assign(q, newChunk),
        assign(chunk3loc, newChunk),
        //Comment("HERE!!"),
        writeField(v(p), f, v(q)),
        writeField(v(q), f, v(chunk3loc)),
        writeField(v(chunk3loc), f, v(p)),
        write(q, Reg.zero),
        write(chunk3loc, Reg.zero),
        //writeField()
        //writeField(v(q), f, v(p)),
        //assign(p, v(q)),
        call(GarbageCollector.collectGarbage),
        call(GarbageCollector.collectGarbage),
        //call(GarbageCollector.collectGarbage),
        //call(GarbageCollector.collectGarbage),
        ADD(Reg.zero, Reg.zero)
      ))
      val machineCode = compilerA6(main +: GarbageCollector.procedures)
      val finalState = A4.loadAndRun(machineCode, debug = true)
      println("reachable bytes: " + decodeSigned(finalState.reg(3)).toInt)

      def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
        if (words > 0) {
          println(decodeUnsigned(address) +  " - " +  spaces(address) + ": " + spaces(state.mem(address)) + " - " + decodeSigned(state.mem(address)))
          dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words - 1)
        }
      }

      def spaces(w: Word): String = w.toString.sliding(8, 8).mkString(" ")

      //println("chunk")
      //dumpMem(finalState, Word(encodeUnsigned(16777116)), 11)
      println("Semispace 1")
      dumpMem(finalState, GarbageCollector.heapStart, 9)
      println("Semispace 2")
      dumpMem(finalState, GarbageCollector.heapMiddle, 9)
      println("heapPtr: " + spaces(finalState.reg(Reg.heapPointer.number)))
    }
  }

  def printState(s: State): Unit = {
    for(i <- 1 to 31){
      println(i + ": " + Assembler.decodeUnsigned(s.reg(i)))
    }
    println(s.reg(Reg.targetPC.number))
  }
}