/*
   Copyright 2022 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2022 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2022. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
*/
package cs241e.assignments

import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.mips._

/** An implementation of a strategy to lay out and keep track of memory, both on the stack and on the heap. */

object MemoryManagement {

  /** To make it easier to keep track of which areas of memory are used to store which data, we
    * organize all memory in `Chunk`s. A `Chunk` is a compile-time representation of multiple
    * words of memory indexed by a sequence of `Variable`s. Each variable is assigned a fixed
    * offset from the beginning of the `Chunk`. The `Chunk` can generate the
    * code to store and load the value at the offset correxponding to each variable.
    *
    * When a `Chunk` is represented in memory at run time, word 0 of the `Chunk`
    * always holds the size of the `Chunk` in bytes. This makes it possible to deallocate
    * or copy the run-time instance of the `Chunk` in memory without knowing the specific
    * static `Chunk` that it represents. Word 1 of the `Chunk` is reserved for Assignment 11
    * (discussed in the next paragraph). To summarize, the memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: reserved for Assignment 11
    * words 2 to n+1: variables
    *
    * Starting in Assignment 11, it is also necessary to know at run time which `Variable`s
    * in the run-time `Chunk` are pointers to other `Chunk`s (have their isPointer == true).
    * To enable this, word 1 of the
    * `Chunk` always holds the number of pointer `Variable`s in the chunk. In addition,
    * all of the pointer `Variable`s are allocated first, starting from word 2
    * of the `Chunk`, followed by all of the non-pointer variables. To summarize, the
    * full memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: p = number of pointer variables
    * words 2   to 2+p-1: pointer variables
    * words 2+p to 2+n-1: non-pointer variables
    */
  case class Chunk(variables: Seq[Variable]) {
    /** For Assignment 11, the variables need to be sorted as described above, so we sort them here.
      * For earlier assignments, the order of the variables is irrelevant so sorting them here doesn't affect anything.
      */
    private val sortedVariables: Seq[Variable] = {
      val (pointers, nonPointers) = variables.partition(_.isPointer)
      pointers ++ nonPointers
    }

    /** The amount of memory needed to store the chunk in words. */
    val words: Int = 2 + variables.size
    /** The amount of memory needed to store the chunk in bytes. */
    val bytes: Int = words * 4
    
    /** Maps each variable to the offset of its address from the start of the `Chunk`.
      *
      * Scala hint:
      *
      * Seq('a', 'b', 'c')
      *   .zipWithIndex.map{case (letter, index) => (letter, index*2)}
      *   .toMap ==
      * Map('a' -> 0, 'b' -> 2, 'c' -> 4)
      *
      */
    private val variableToOffset: Map[Variable, Int] = sortedVariables.zipWithIndex.map{
      // adding two since first two blocks of chunk are taken
      case (letter,index) => (letter, index*4 + 8)
    }.toMap



    /** Generate code to load the value at the offset corresponding to `variable` into `register`.
      *
      * Assume that `baseRegister` contains the address of the beginning of the chunk.
      **/
    def load(baseRegister: Reg, register: Reg, variable: Variable): Code = {
      variableToOffset.get(variable) match {
        case Some(offset) => block(
          Comment("load: " + variable.name + register + offset + baseRegister),
          LW(register, offset, baseRegister)
        )
        case None => sys.error("Trying to load variable not in chunk: " + variable.name)
      }
    }

    /** Generate code to store the value of `register` at the offset corresponding to `variable`.
      *
      * Assume that `baseRegister` contains the address of the beginning of the chunk.
      **/
    def store(baseRegister: Reg, variable: Variable, register: Reg): Code = {
      variableToOffset.get(variable) match {
        case Some(offset) => block(
          Comment("store: " + variable.name),
          SW(register, offset, baseRegister)
        )
        case None => sys.error("Trying to store variable not in chunk: " + variable.name)
      }
    }

    /** Generate code to initialize a `Chunk` that has just been allocated. The generated code should
      * assume that register `Reg.result` contains the address of the beginning of the `Chunk`.
      * It should write the size of the `Chunk` in bytes into word 0 of the `Chunk`.
      * It should set the values of all of the variables in the `Chunk` to 0.
      *
      * The generated code may modify the values of Reg.scratch. If you need more than one scratch
      * register, you may add new scratch registers to Reg.scala. The generated code must not modify the values
      * of any registers that are already listed in Reg.scala (except for Reg.scratch).
      *
      * Starting in Assignment 11, the generated code should also write the number of pointer variables into
      * word 1 of the `Chunk`.
      */
    def initialize: Code = {
      val loads = variables.map(v => {
        variableToOffset.get(v) match {
          case Some(offset) => SW(Reg.zero, offset, Reg.result)
          case None => sys.error("Trying to init chunk with invalid variable: " + v.name)
        }
      })
      val numberOfPointers = variables.foldLeft(0)((x, y) => x + (if (y.isPointer) 1 else 0))
      println("number of pointers: ", numberOfPointers, "vars: ", variables)

      Block(
        Seq(Comment("Start of block init"))
        ++
        (
          Seq(
            LIS(Reg.scratch),
            Word(encodeUnsigned(bytes)),
            SW(Reg.scratch, 0, Reg.result),
            LIS(Reg.scratch),
            Word(encodeUnsigned(numberOfPointers)),
            SW(Reg.scratch, 4, Reg.result),
          ) ++ loads
        ).map(w => CodeWord(w))
      )
    }
  }

  /** An abstract memory allocator that allocates memory either on the stack or on the heap. */
  abstract class MemoryAllocator {
    /** The code to initialize the memory allocator at the beginning of the program. By default,
      * no initialization is necessary.
      */
    val initCode: Code = block()

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      */
    def allocate(chunk: Chunk): Code
  }

  /** A `MemoryAllocator` that allocates `Chunk`s of memory on the stack. */
  object Stack extends MemoryAllocator {
    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * The generated code may modify the values of Reg.stackPointer, Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * and Reg.scratchPtrForGC. If you need more than these registers, you may add new scratch
      * registers to Reg.scala. The generated code must not modify the values of any other registers that are
      * already listed in Reg.scala.
      */
    def allocate(chunk: Chunk): Code = block(
      Comment("Start of chunk allocation"),
      // load size of chunk in scratch
      LIS(Reg.scratch),
      Word(encodeUnsigned(chunk.bytes)),

      // move stack pointer to end of chunk
      SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      ADD(Reg.result, Reg.stackPointer, Reg.zero), // store address of start of chunk in Reg.result

      chunk.initialize,
      Comment("End of chunk allocation")
    )
    /** Generate the code to deallocate the space for the `Chunk` that is at the top of the stack. To determine
      * the size of this `Chunk`, takes advantage of the convention that word 0 of each `Chunk` stores its size
      * in bytes.
      *
      * The generated code must not modify Reg.result. It may modify Reg.stackPointer and Reg.scratch.
      * If you need more than these registers, you may add new scratch registers to Reg.scala. The generated code
      * must not modify the values of any other registers that are already listed in Reg.scala.
      */
    val pop: Code = block(
      Comment("Start of pop"),
      LW(Reg.scratch, 0, Reg.stackPointer), // load size of chunk
      ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Comment("End of pop")
    )
  }

  /** Code that copies a chunk whose address is in `fromRegister` to the address in `toRegister`.
    * `toRegister` and `fromRegister` cannot be one of the registers in `modifiedRegisters`.
    * Be careful to modify only the registers in `modifiedRegisters` in the copying code that
    * you generate. If you need to modify additional registers, add them to the `modifiedRegisters`
    * set in order to be notified if you call `copyChunk` with one of these registers as `toRegister`
    * or `fromRegister`.
    *
    * Also, do not use any Variables inside copyChunk, or any Code that depends on
    * them, that is, any code appearing after Block in ProgramRepresentation.scala,
    * particularly including whileLoop. This is because copyChunk will be used to
    * implement calls from one procedure to another, and it is not clear in which
    * procedure's frame such Variables are allocated.
    */
  def copyChunk(toRegister: Reg, fromRegister: Reg): Code = {
    /* The registers that may be modified by the code that will be generated. */
    val modifiedRegisters = Set(Reg.scratch, Reg.copyChunkScratch, Reg.scratchForStaticLink)
    require(!modifiedRegisters.contains(toRegister))
    require(!modifiedRegisters.contains(fromRegister)) // 8
    val startLabel = new Label("copy chunk start");
    val endLabel = new Label("copy chunk end");

    block(
      ADD(Reg.scratch, Reg.zero), // scratch is counter, counter starts at 0
      Define(startLabel),
      LW(Reg.copyChunkScratch, 0, fromRegister), // copyChunkScratch has size now
      SLTU(Reg.copyChunkScratch, Reg.scratch, Reg.copyChunkScratch), // now ccs is 1 if copyChunkScrath < scratch, else 0
      // if scratch < copChunkScartch, 1
      // if scratch == copychunkscratch 0
      // if scratch > copychunkscratch 0
      beq(Reg.copyChunkScratch, Reg.zero, endLabel),
      ADD(Reg.copyChunkScratch, fromRegister, Reg.scratch), // copyChunkScratch has fromRegister + counter address now
      LW(Reg.copyChunkScratch, 0, Reg.copyChunkScratch), // load that value
      ADD(Reg.scratchForStaticLink, toRegister, Reg.scratch ), // load targetRegister + counter
      SW(Reg.copyChunkScratch, 0, Reg.scratchForStaticLink),// store the value
      LIS(Reg.copyChunkScratch),
      Word(encodeUnsigned(4)),
      ADD(Reg.scratch, Reg.scratch, Reg.copyChunkScratch),
      beq(Reg.zero, Reg.zero, startLabel),
      Define(endLabel)
    )
  }

  private var heapImplementation: MemoryAllocator = SimpleHeapAllocator
  def heap: MemoryAllocator = heapImplementation

  trait HeapSettings {
    /** The total number of bytes of memory. */
    val memSize = decodeUnsigned(CPU.maxAddr)
    /** The address of the beginning of the heap. */
    val heapStart = Word(encodeUnsigned(memSize / 4))
    /** The address of the middle of the heap. */
    val heapMiddle = Word(encodeUnsigned(memSize / 2))
    /** The address just after the end of the heap. */
    val heapEnd = Word(encodeUnsigned(memSize * 3 / 4))
  }

  /** A simple `MemoryAllocator` that allocates `Chunk`s of memory on the heap in a way that they
    * are never freed. Specifically, `Reg.heapPointer` is assumed to point to the next unused
    * memory address in the heap. To allocate a `Chunk` of a given size, the allocator just returns
    * the current value of `Reg.heapPointer` and increments it by the size so that it points to the
    * next unused word.
    */
  private object SimpleHeapAllocator extends MemoryAllocator with HeapSettings {
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart)

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`.
      */
    def allocate(chunk: Chunk): Code = block(
      LIS(Reg.scratch),
      Word(encodeUnsigned(chunk.bytes)),
      ADD(Reg.result, Reg.heapPointer),
      ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),
      chunk.initialize
    )
  }

  /* ## Assignment 11 */

  /** A `MemoryAllocator` that uses a copying garbage collector to reclaim `Chunk`s that are unreachable
    * from the current `Chunk` whose address is in `Reg.framePtr`. The heap is split into two halves (semispaces).
    * Memory is allocated from one of the semispaces. When the semispace becomes full, the garbage collector
    * is launched. The garbage collector copies all reachable `Chunk`s into the other semispace
    * and adjusts all pointer `Variable`s in all reachable `Chunk`s to point to the new copies. The other semispace
    * then becomes the current semispace from which memory is allocated until it becomes full, and then the whole
    * process is repeated.
    *
    * The provided `initCode` follows the assumption that `Reg.heapPointer` points to the next unused
    * word in the current semispace, and that `Reg.fromSpaceEnd` points to the word immediately
    * after the end of the current semispace.
    *
    * The first semispace starts at address heapStart and ends just before heapMiddle, and
    * the second semispace starts at address heapMiddle and ends just before heapEnd.
    */

  object GarbageCollector extends MemoryAllocator with HeapSettings {
    
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart, LIS(Reg.fromSpaceEnd), heapMiddle)

    /** Declarations of procedures of the memory allocator and of local variables used in those procedures.
      *
      * You may add more procedures and variables here. If you add procedures, be sure to add them to
      * `def procedures` below.
      */
    
    val allocateProc_bytes = new Variable("allocateProc_bytes")
    val allocateProc = new Procedure("allocateProc", Seq(allocateProc_bytes))

    val collectGarbage = new Procedure("collectGarbage", Seq())

    val forwardPtrBlock = new Variable("forwardPtrBlock")
    val forwardPtr = new Procedure("forwardPtr", Seq(forwardPtrBlock), outer = Option(collectGarbage))

    val copyBlock = new Variable("copyBlock")
    val copy = new Procedure("copy", Seq(copyBlock), outer = Option(collectGarbage))


    /** The sequence of all procedures required by memory allocator. This sequence must
      * contain `allocateProc` and `collectGarbage`, as well as any additional helper procedures that you define.
      */
    def procedures: Seq[Procedure] = Seq(allocateProc, collectGarbage, forwardPtr, copy)
    
    /** Code of the `allocateProc` procedure, which allocates an area of `allocateProc_bytes` consecutive bytes in memory
      * and places the address of the beginning of that area in `Reg.result`.
      *
      * If there is not enough space in the current semispace, `allocateProc` should call `collectGarbage` to try to
      * free up space. If there is still not enough space after the garbage collection pass, the behaviour of
      * `allocateProc` is undefined.
      */
    allocateProc.code = {
      block(
        ifStmt(
          binOp(ADD(Reg.result, Reg.heapPointer), plus, readVarRes(allocateProc_bytes)),
          ltCmp,
          ADD(Reg.result, Reg.fromSpaceEnd),
          block(
            // TODO: Remove this
            Call(collectGarbage, Seq())
          ),
          block(
            Call(collectGarbage, Seq())
          )
        ),
        Comment("Returning heap pointer"),
        ADD(Reg.result, Reg.heapPointer),
        read(Reg.scratch, allocateProc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),
      )
    }

    /** Generate the code to call the `allocateProc` procedure to allocate enough memory to hold `chunk` and place
      * the address of the allocated `Chunk` in `Reg.result`. This code should be followed by the code
      * generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`. In particular, it cannot contain the `Call` `Code`, so the code to call
      * the `allocateProc` procedure must be implemented directly in terms of simpler `Code`s.
      */
    def allocate(chunk: Chunk): Code = {
      val paramChunk = new Chunk(allocateProc.staticLink +: allocateProc.parameters)
      block(
        Stack.allocate(paramChunk),
        LIS(Reg.scratch),
        Word(encodeUnsigned(99)), // putting dummy value for static link
        paramChunk.store(Reg.result, allocateProc.staticLink, Reg.scratch ),
        LIS(Reg.scratch),
        Word(encodeUnsigned(chunk.bytes)),
        paramChunk.store(Reg.result, allocateProc_bytes, Reg.scratch),
        LIS(Reg.link),
        Use(allocateProc.label),
        JALR(Reg.link),
        chunk.initialize
      )
    }

    /** The code of the procedure that performs a garbage collection pass over the heap. The procedure should take
      * zero parameters, and it should return in Reg.result an `Int`, the total number of bytes of memory
      * in the heap (that is, the sum of the sizes in bytes of all of the live objects remaining in the heap
      * after the garbage collection pass).
      *
      * You may assume that the `collectGarbage` procedure will only ever be called from a procedure whose frame
      * is stored on the stack, not on the heap. Thus, you may assume that the dynamic link in the `collectGarbage`
      * procedure points to what was the top of the stack before the `collectGarbage` procedure was called.
      */

    def size(block: Code) = deref(block)
    def next(block: Code) = deref(binOp(block, plus, constRes(4)))
    def setSize(block: Code, size: Code) = assignToAddr(block, size)
    def setNext(block: Code, next: Code) = assignToAddr(binOp(block, plus, constRes(4)), next)

    val toSpaceStart = new Variable("toSpaceStart")
    val free = new Variable("free")
    val scan = new Variable("scan")
    collectGarbage.code = Scope(Seq(toSpaceStart, free, scan), block(
      ifStmt(ADD(Reg.result, Reg.fromSpaceEnd), eqCmp, block(
        LIS(Reg.result),
        heapMiddle
      ), block(
        Comment("toSpaceStart is middle, setting"),
        LIS(Reg.result),
        heapMiddle,
        write(toSpaceStart, Reg.result)
      ), block(
        Comment("toSpaceStart is start of heap, settign"),
        LIS(Reg.result),
        heapStart,
        write(toSpaceStart, Reg.result)
      )),
      assign(free, read(Reg.result, toSpaceStart)),
      assign(scan, read(Reg.result, collectGarbage.dynamicLink)),
      whileLoop(readVarRes(scan), ltCmp, block(LIS(Reg.result), CPU.maxAddr), block(
        Comment("First while loop"),
        Call(forwardPtr, Seq(block(read( Reg.result, scan)))),
        // scan = scan + scan(size)
        Comment("scan = scan + size(scan)"),
        size(read(Reg.result, scan)),
        read(Reg.scratch, scan),
        ADD(Reg.result, Reg.result, Reg.scratch),
        write(scan, Reg.result)
      )),

      read(Reg.result, toSpaceStart),
      write(scan, Reg.result),

      whileLoop(readVarRes(scan), ltCmp, read(Reg.result, free), block(
        Comment("second while loop"),
        Call(forwardPtr, Seq(block(read( Reg.result, scan)))),
        // scan = scan + scan(size)
        Comment("scan = scan + size(scan)"),
        size(read(Reg.result, scan)),
        read(Reg.scratch, scan),
        ADD(Reg.result, Reg.result, Reg.scratch),
        write(scan, Reg.result)
      )),

      Comment("fromSpaceEnd = toSpaceStart + semiSpaceSize"),
      binOp(read(Reg.result, toSpaceStart), plus,
        binOp(
          block(LIS(Reg.result), heapMiddle), minus,
          block(LIS(Reg.result), heapStart)
        )),
      ADD(Reg.fromSpaceEnd, Reg.result),
      read(Reg.heapPointer, free),

      // store number of fre
      Comment("storing number items used in heap in Reg.result"),
      binOp(read(Reg.result, free), minus, read(Reg.result, toSpaceStart)),
    ))


    val numberOfPtrs = new Variable("numberOfPointers");
    val current = new Variable("current");
    val tmp = new Variable("tmp-stores-newAddr");
    forwardPtr.code = Scope(Seq(numberOfPtrs, current, tmp), block(
      Comment("In forward ptrs"),
      Comment("writing number of ptrs before"),
      deref(
        binOp(read(Reg.result, forwardPtrBlock), plus, constRes(4)),
      ),
      Comment("writing number of ptrs"),
      write(numberOfPtrs, Reg.result),
      assign(current, ADD(Reg.result, Reg.zero)),


      whileLoop(read(Reg.result, current), ltCmp, read(Reg.result, numberOfPtrs), block(

        Comment("inside forward"),
        Comment("here is currentChunk we're forwarding"),
        read(Reg.result, forwardPtrBlock),
        Comment("Current is"),
        read(Reg.result, current),
        Comment("number of ptrs is"),
        read(Reg.result, numberOfPtrs),
        binOp(binOp(
          binOp(read(Reg.result, current), times, constRes(4)),
          plus, read(Reg.result, forwardPtrBlock)), plus, constRes(8)),
        Comment("doing deref of current*4 + forwardPtrBlock + 8"),

        /*
        LIS(Reg.link),
        Word(encodeSigned(-18751827)),
        JR(Reg.link),
        */
        //write(tmp, Reg.result),

        //ifStmt(read(Reg.result, tmp), neCmp, ADD(Reg.result, Reg.zero), block(
        //read(Reg.result, tmp),
          deref(ADD(Reg.result, Reg.result)),
          Call(copy, Seq(ADD(Reg.result, Reg.result))),
          write(tmp, Reg.result),
          assignToAddr(
            binOp(binOp(
              binOp(read(Reg.result, current), times, constRes(4)),
              plus, read(Reg.result, forwardPtrBlock)), plus, constRes(8)),
            read(Reg.result, tmp)
          ),
        //)),

        assign(current, binOp(read(Reg.result, current), plus, constRes(1)))
      ))
    ))

    val isInFromSpace = new Variable("isInFromSpace");
    copy.code = Scope(Seq(isInFromSpace), block(
      write(isInFromSpace, Reg.zero),
      ifStmt(ADD(Reg.result, Reg.fromSpaceEnd), eqCmp, block(LIS(Reg.result), heapMiddle),
        block(
          Comment("fromSpaceEnd is heapMiddle"),
          ifStmt(read(Reg.result, copyBlock), ltCmp, ADD(Reg.result, Reg.fromSpaceEnd), block(
            ifStmt(read(Reg.result, copyBlock), geCmp, block(LIS(Reg.result), heapStart), block(
              LIS(Reg.result),
              Word(encodeUnsigned(1)),
              write(isInFromSpace, Reg.result),
              Comment("setIsInFromSpace true")
            ))
          ))
        ),
        block(
          Comment("fromSpaceEnd is heapEnd"),
          ifStmt(read(Reg.result, copyBlock), ltCmp, ADD(Reg.result, Reg.fromSpaceEnd), block(
            ifStmt(read(Reg.result, copyBlock), geCmp, block(LIS(Reg.result), heapMiddle), block(
              LIS(Reg.result),
              Word(encodeUnsigned(1)),
              write(isInFromSpace, Reg.result),
              Comment("setIsInFromSpace true")
            ))
          ))
        )
      ),
      ifStmt(read(Reg.result, isInFromSpace), eqCmp, ADD(Reg.result, Reg.zero), block(
        Comment("isInFromSpace is false"),
        read(Reg.result, copyBlock)
      ), block(
        Comment("isInFromSpace is true"),
        ifStmt(size(read(Reg.result, copyBlock)), geCmp, ADD(Reg.result, Reg.zero), block(
          read(Reg.result, free),
          read(Reg.targetPC, copyBlock),
          copyChunk(Reg.result, Reg.targetPC),
          setNext(read(Reg.result, copyBlock), read(Reg.result, free)),
          setSize(read(Reg.result, copyBlock), binOp(ADD(Reg.result, Reg.zero), minus, size(read(Reg.result, copyBlock)))),
          binOp(read(Reg.result, free), plus, size(read(Reg.result, free))),
          write(free, Reg.result),
        )),
        next(read(Reg.result, copyBlock)),
      ))
    ))
  }

  /** Runs the provided Scala code block using the GarbageCollector implementation of the heap. */
  def withGC[T](code: =>T): T = withHeap(GarbageCollector, code)

  def withHeap[T](newHeap: MemoryAllocator, code: =>T): T = try {
    heapImplementation = newHeap
    code
  } finally { heapImplementation = SimpleHeapAllocator }
}

/*

TUT Notes:
Modify allocateProc to always call collectGarbage
lactsProgramStress marmoset test does this

make sure to not call GC as a tail call


heaps only test:
  test(""){
    withGC {
        val f = new Variable("f", isPointer = true)
        val chunk = Chunk(Seq(f));
        def newChunk = heap.allocate(chunk);
        val main = new Procedure("main", Seq(a, b))
        val p = new Variable("p", isPointer = true);
        main.code = Scope(seq(p), block(
          assign(p, newChunk);
        ))

        val machineCode = compilerA6(main +: GatbageCollecter.procedures)
        val finalState = A4.loadAndRun(machineCode)
        println(decodeSigned(finalState.reg(3)).toInt)

        def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
          if(words > 0){
            println(spaces(address + ": " + state.mem(address)));
            dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words - 1)
          }
        }

        def spaces(w: Word): String = w.toString.sliding(8,8).mkstring(" ")
        println("semispace 1")
        dumpMem(finalState, GargabeCollector.heapStart)
        println("semispace 2")
        dumpMem(finalState, GargabeCollector.heapMiddle)
        println("heapPtr: " + finalState.reg(Reg.heapPointer.number))
    }
  }












 */