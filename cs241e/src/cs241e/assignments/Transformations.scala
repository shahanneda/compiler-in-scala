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

import ProgramRepresentation._
import CodeBuilders._
import Assembler._
import MemoryManagement._
import cs241e.mips._
import cs241e.Utils._
import Debugger._
import cs241e.assignments.Transformations.closureChunk

import scala.:+
import scala.collection.mutable

/** Implementations of various transformations on the `Code` objects defined in `ProgramRepresentation.scala`.
  * In general, the transformations successively eliminate various types of `Code` objects by translating them
  * into sequences of simpler `Code`s, until only `Code`s directly representing machine instructions are left.
  */

object Transformations {
  /* ############################################################### */
  /* ## Assignment 2 ############################################### */
  /* ############################################################### */

  /* Before doing Assignment 2, read the code in the first half of `ProgramRepresentation.scala`
   * (up to the `Block` case class) to get an idea of how we will represent programs using the various subclasses
   * of the `Code` class.
   *
   * Hint: You can navigate to the `Code` class by Ctrl-clicking on any use of the word `Code`.
   */

  /* Complete the implementation of the following method by replacing the `???`. */

  /** Given a sequence of `Code`s that may be one of `CodeWord`, `Define`, `Use`, or `BeqBne`,
    * resolve all of the labels, and output the corresponding MIPS machine-language program
    * as a sequence of `Word`s.
    *
    * Refer to `ProgramRepresentation.scala` for documentation of the meanings of these `Code`s.
    *
    * If a label is defined multiple times or if a label is used but not defined, call
    * `sys.error()` with an appropriate error message.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The high-level structure of this method is given for you, but you should learn about
    * methods such as `foreach` and `flatMap` because you may want to use them yourself in
    * later assignments.
    */
  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val symbolTable = mutable.Map[Label, Int]()

    /* First pass: fill in the `symbolTable` map with an address for each label. */
    def setLabels(): Unit = {
      var address = 0
      code.foreach {
        case Define(label) => {
          if(symbolTable.contains(label)){
            sys.error("Duplicate label: " +  label)
          }
          symbolTable(label) = address
        }
        case _ => address += 4
      }
    }

    /* Second pass: replace each `Code` with an equivalent sequence of `Word`s. */
    def translate: Seq[Word] = {
      var location = 0
      code.flatMap {
        case Define(label) => None
        case CodeWord(word) => {
          location += 1
          Seq(word)
        }
        case Use(label) => {
          symbolTable.get(label) match {
            case Some(address) =>{
              location += 1
              Seq(Word(encodeUnsigned(address)))
            }
            case None =>{
              sys.error(s"Undefined label $label !")
              Seq(Word("10101010101010"))
            }
          }
        }
        case BeqBne(bits, label) => symbolTable.get(label) match{
          case Some(address) => {
            location += 1 // since PC is already incremented, we are actualy at the next location
            val out =  Seq(Word(bits ++ encodeSigned(address/4 - location, 16)))
            out
          }
          case None => {
            sys.error(s"Undefined label $label !")
            Seq(Word.zero)
          }
        }
        case _ => impossible(s"Encountered unsupported code $code.")
      }
    }

    setLabels()
    translate
  }

  /** Links two sequences of code together to form a single program. */
  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2

  /** Remove all `Comment`s from a sequence of `Code`s.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `Comment` in `ProgramRepresentation.scala`.
    */
  def eliminateComments(codes: Seq[Code]): Seq[Code] = codes.flatMap{
    case Comment(_) => None
    case anything => Seq(anything)
  }

  /** Eliminate all `Block`s from a tree of `Code` by flattening the tree into a sequence of
    * `Code`s other than `Block`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `Block` in `ProgramRepresentation.scala`.
    */
  def eliminateBlocks(code: Code): Seq[Code] = code match {
    case Block(children) => children.flatMap(c => eliminateBlocks(c))
    case anything => Seq(anything)
  }

  /** Transform a `Code` tree by applying the function `fun` to transform each node of the tree of type `Code`.
    *
    * More specifically, at a given node `code`, first recursively call `transformCodeTotal` to transform each of
    * the children of the node. Then apply `fun` on the resulting node whose children have been transformed.
    *
    * Note: `transformCodeTotal` should handle **all** the various possible subclasses of `Code`, not only the ones that
    * we have used so far.
    */
  def transformCodeTotal(code: Code, fun: Code=>Code): Code = {
    /** Apply `transformCodeTotal` on all child code nodes of the argument code node `code`. */
    def processChildren(code: Code): Code = code match {
      case Block(children) => Block(children.map(c => fun(transformCodeTotal(c, fun))))
      case Scope(variables, body) => Scope(variables, fun(transformCodeTotal(body, fun)))
      case IfStmt(elseLabel, e1, comp, e2, thens, elses) =>
        IfStmt(elseLabel,
          fun(transformCodeTotal(e1, fun)),
          fun(transformCodeTotal(comp, fun)),
          fun(transformCodeTotal(e2, fun)),
          fun(transformCodeTotal( thens, fun)),
          fun(transformCodeTotal(elses, fun)))
      case Call(procedure, args, isTail) => Call(procedure, args.map(c => fun(transformCodeTotal(c, fun))), isTail)
      case CallClosure(procedure, args, params, isTail) =>
        CallClosure(procedure,
        args.map(c => fun(transformCodeTotal(c, fun))),
          params,
          isTail)
      case _ => code
    }

    fun(processChildren(code))
  }

  /** A adaptation of `transformCodeTotal` to transform code trees with a partial function. If the partial
    * function is not defined for a particular tree node, that tree node is left unmodified.
    *
    * Scala Hint: Read
    * https://www.scala-lang.org/api/current/scala/PartialFunction.html
    * for an explanation of `PartialFunction`.
    */
  def transformCode(code: Code, fun: PartialFunction[Code, Code]): Code = {
    transformCodeTotal(code, code => if(fun.isDefinedAt(code)) fun(code) else code)
  }

  /* ############################################################### */
  /* ## Assignment 3 ############################################### */
  /* ############################################################### */

  /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
    * reading or writing the relevant variable.
    *
    * To do this, we need an activation record or `frame` containing the `Variable`s used in `code` that
    * determines the address in memory where each `Variable` is stored, as an offset from the address in
    * `Reg.framePointer`. See the definition of the `Chunk` class in `MemoryManagement.scala` for details.
    *
    * Hint: the `transformCode` method is very helpful for transforming `Code` in general, and for eliminating
    * `VarAccess`es specifically.
    *
    * Scala Hint: {case va: VarAccess => ???} defines a `PartialFunction` that is defined for `VarAccess`es
    * but is not defined for any other type of `Code`.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement a variable access may modify the value of Reg.scratch.
    * If the access is a read, it may also of course modify the target register. If you need more
    * than these registers, you may add new scratch registers to Reg.scala. The generated code must
    * not modify the values of any other registers that are already listed in Reg.scala.
    */
  def eliminateVarAccessesA3(code: Code, frame: Chunk): Code = {
    def fun: PartialFunction[Code, Code] = {
      case va: VarAccess =>
        if(va.read){
          frame.load(Reg.framePointer, va.register, va.variable)
        }
        else{
          frame.store(Reg.framePointer, va.variable, va.register)
        }
    }
    transformCode(code, fun)
  }

  /** Given a `body` of `Code` and a `frame` of variables that it uses, generates
    * code that allocates space for the `frame` on the stack and sets `Reg.framePointer` to
    * point to it, followed by the `body`, followed by code to free the space for the `frame` from the stack.
    */
  def allocateFrameOnStack(body: Code, frame: Chunk): Code =
    block(
      Comment("Start of allocate frame"),
      Stack.allocate(frame),
      ADD(Reg.framePointer, Reg.result, Reg.zero), // Store new Frame pointer
      Comment("End of frame allocation, start body:"),
      body,
      Comment("End of body, pop:"),
      Stack.pop
    )

  /** A bundle of machine language code in the form of a sequence of `Word`s and a `debugTable` for the `Debugger`. */
  case class MachineCode(words: Seq[Word], debugTable: DebugTable)

  /** Given a `Code` tree containing only `Code` types that are defined before `Block` in `ProgramRepresentation.scala`,
    * successively eliminates all `Code` types to yield just a sequence of `Word`s representing the equivalent program
    * in machine language. In addition, generates a `DebugTable` for that program.
    */
  def toMachineCode(code: Code): MachineCode = {
    val code2 = eliminateBlocks(code)
    val debugTable = createDebugTable(code2)
    val code3 = eliminateComments(code2)
    val code4 = eliminateLabels(code3)
    MachineCode(code4, debugTable)
  }

  /** Given a `Code` tree that may contain any `Code` types defined in `ProgramRepresentation.scala` up to and
    * including `VarAccess`, successively eliminates all `Code` types to yield just a sequence of `Word`s
    * representing the equivalent program in machine language. In addition, generates a `DebugTable` for that program.
    *
    * Requires a sequence of all `Variable`s that are accessed within `code`. The generated machine language code
    * will allocate space for the variables on the stack and free it at the end.
    */
  def compilerA3(code: Code, variables: Seq[Variable]): MachineCode = {
    val frame = Chunk(variables)
    val code1 = eliminateVarAccessesA3(code, frame)
    val code2 = allocateFrameOnStack(code1, frame)
    toMachineCode(block(code2, JR(Reg.link)))
  }

  /* ############################################################### */
  /* ## Assignment 4 ############################################### */
  /* ############################################################### */

  /** Eliminate all `Scope`s from a tree of `Code` by simply returning the `code` field
    * for each one.
    *
    * Return a pair of the resulting code and a sequence of the temporary `Variable`s extracted
    * from the `Scope`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `Scope` in `ProgramRepresentation.scala`.
    *
    * Hint: Use `transformCode`.
    */
  def eliminateScopes(code: Code): (Code, Seq[Variable]) = {
    var vars: Seq[Variable] = Seq()

    def f: PartialFunction[Code, Code] = {
      case Scope(variables, code) => {
        vars = variables ++ vars
        code
      }
    }

    val newCode = transformCode(code, f)

    (newCode, vars)
  }

  /** Eliminate all `IfStmt`s from a tree of `Code` by translating them to simpler pieces
    * of `Code`.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `IfStmt` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement an if statement may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def eliminateIfStmts(code: Code): Code = {
    def fun: PartialFunction[Code, Code] = {
      case IfStmt(elseLabel, e1, comp, e2, thens, elses) => {
        val e1Val = new Variable("e1 ");
        val endIfLabel = new Label("End If")
        Scope(
          Seq(e1Val),
          block(
            e1,
            write(e1Val, Reg.result),
            e2,
            read(Reg.scratch, e1Val),
            Comment("If Comp:"),
            comp,
            Comment("Then code:"),
            thens,
            beq(Reg.zero, Reg.zero, endIfLabel),
            Define(elseLabel),
            elses,
            Define(endIfLabel),
          )
        )
      }
    }

    transformCode(code, fun)
  }

  def compilerA4(code: Code): MachineCode = {
    val code1 = eliminateIfStmts(code)
    val (code2, variables) = eliminateScopes(code1)
    assert(variables.distinct == variables)
    compilerA3(code2, variables)
  }

  /* ############################################################### */
  /* ## Assignment 5 ############################################### */
  /* ############################################################### */

  /** For each `Variable` in the sequence `params`, creates a new variable (intended to be used as a
    * temporary variable). Returns a sequence of temporary variables of the same length as the input
    * sequence `params`. For each `Variable` in `params`, if the `isPointer` flag of the variable is
    * set to true, then the `isPointer` flag of the temporary variable at the same position in the returned
    * sequence will also be set to true.
    */
  def createTempVars(params: Seq[Variable]): Seq[Variable] =
    params.map(param => new Variable("temp-" + param.name, param.isPointer))

  /** Given a set of `keys` for a map and a `function`, applies the function to each key, and stores
    * the result in a `Map` from `keys` to the `function` values.
    */
  def makeMap[A, B](keys: Seq[A], function: A=>B): Map[A, B] = keys.map(key => (key, function(key))).toMap

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */
  def compilerA5(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters of each procedure. */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters))

    /** Compile a single procedure to machine language. */
    def compileProcedure(currentProcedure: Procedure): Code = {

      /** Eliminate all `Call`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * The general strategy for passing arguments is as follows:
        * - create temporary variables, one for each argument
        * - evaluate the arguments, storing them in the temporary variables
        * - allocate memory for the parameter `Chunk`
        * - copy the values of the temporary variables to the parameter `Chunk`
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `Call` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case call: Call =>
            val caller = currentProcedure
            val callee = call.procedure
            val tempVars = createTempVars(call.procedure.parameters).toList
            val paramChunk = paramChunks.get(call.procedure) match {
              case Some(value) => value
              case None => sys.error("Param chunk not found for procedure with name " + call.procedure.name)
            }
            Scope(tempVars, block(
              Comment("Start of call to " +  callee.name + " from " + caller.name),
              Block(call.arguments.zipWithIndex.map{ case (argCode, i) => {
                block(
                  argCode,
                  write(tempVars(i), Reg.result),
                )
              }}),
              Stack.allocate(paramChunk),
              Block(
                paramChunk.variables.zipWithIndex.map{case (paramVar, i) =>
                  block(
                    read(Reg.scratch, tempVars(i)),
                    paramChunk.store(Reg.result, paramVar, Reg.scratch)
                  )
                },
              ),
              LIS(Reg.targetPC),
              Use(callee.label),
              JALR(Reg.targetPC),
            ))
        }

        transformCode(code, fun)
      }

      /* First part of main body of compileProcedure. */
      val code1 = eliminateCalls(currentProcedure.code)
      val code2 = eliminateIfStmts(code1)
      val (code3, variables) = eliminateScopes(code2)
      assert(variables.distinct == variables)

      val frame = Chunk(variables ++
        Seq(currentProcedure.savedPC, currentProcedure.paramPtr, currentProcedure.dynamicLink)
      )

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * The prologue assumes that when the procedure is called, `Reg.result` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk in order to later store it into the `paramPtr` variable
        *   in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the callee
        * - saves `Reg.link` in the `savedPC` variable of the `frame`
        * - stores the saved address of the parameter chunk into the `paramPtr` variable in the procedure `frame`
        *
        * The epilogue:
        * - restores `Reg.link` and `Reg.framePointer` from the current frame
        * - pops the callee's parameter chunk and `frame` off the stack
        * - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has been called. Therefore, this method
        * should not introduce any new `VarAccess`es into the code (by calling `read` or `write`).
        *
        * Hint: this method can use the `frame` from its enclosing method `compileProcedure`.
        */
      def addEntryExit(code: Code): Code = {
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.result), // store param ptr until we make a proper frame
          Stack.allocate(frame),
          frame.store(Reg.result, currentProcedure.savedPC, Reg.link), // store return address
          frame.store(Reg.result, currentProcedure.paramPtr, Reg.savedParamPtr), // store saved param pointer
          frame.store(Reg.result, currentProcedure.dynamicLink, Reg.framePointer), // save callers frame pointer
          ADD(Reg.framePointer, Reg.result), // set the new frame pointer
          Comment("Start of body for " + currentProcedure.name),
        )
        val exit = block(
          Comment("End of body for " + currentProcedure.name),
          frame.load(Reg.framePointer, Reg.link, currentProcedure.savedPC), // load return address
          Comment("Restoring older frame pointer in procedure " + currentProcedure.name),
          frame.load(Reg.framePointer, Reg.framePointer, currentProcedure.dynamicLink), // load old frame pointer
          Comment("after restore " + currentProcedure.name),
          Stack.pop, // pops stack frame
          Stack.pop, // pops params frame
          JR(Reg.link),
        )
        if(code == null){
          sys.error("No body given for procedure: " + currentProcedure.name)
        }
        block(Define(currentProcedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 3, this method needs to handle accesses not only to variables,
        * but also to the parameters of the current procedure.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value of Reg.scratch.
        * If the access is a read, it may also of course modify the target register. If you need more
        * than these registers, you may add new scratch registers to Reg.scala. The generated code must
        * not modify the values of any other registers that are already listed in Reg.scala.
        *
        * Hint: this method can use the `frame` from its enclosing procedure `compileProcedure`.
        */
      def eliminateVarAccessesA5(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case VarAccess(register, variable, read) =>{
            val params = paramChunks.getOrElse(currentProcedure, sys.error("Failed to find params for current procedure"))
            if(frame.variables.contains(variable)){
              if(read){
                frame.load(Reg.framePointer, register, variable)
              }else{
                frame.store(Reg.framePointer, variable, register)
              }
            }else if (params.variables.contains(variable)){
              val paramFrame = paramChunks(currentProcedure)
              if(read){
                block(
                  frame.load(Reg.framePointer, Reg.savedParamPtr, currentProcedure.paramPtr),
                  paramFrame.load(Reg.savedParamPtr, register, variable)
                )
              }else{
                block(
                  frame.load(Reg.framePointer, Reg.savedParamPtr, currentProcedure.paramPtr),
                  paramFrame.store(Reg.savedParamPtr, variable, register)
                )
              }
            }else{
              sys.error("Variable not in params or in chunk! " + variable.name)
            }

          }
        }

        transformCode(code, fun)
      }

      /* Rest of main body of compileProcedure. */

      val code4 = eliminateVarAccessesA5(code3)
      addEntryExit(code4)
    }

    /* Main body of compilerA5. */

    val code = block(
      Stack.allocate(Chunk(procedures.head.parameters)), // allocate parameter chunk for start procedure
      Block(procedures.map(compileProcedure))
    )
    toMachineCode(code)
  }

  def startProcedure(mainProcedure: Procedure): Procedure = {
    val ret = new Procedure("start", Seq())
    ret.code = Call(mainProcedure, Seq(ADD(Reg.result, Reg.input1), ADD(Reg.result, Reg.input2)))
    ret
  }

  /* ############################################################### */
  /* ## Assignment 6 ############################################### */
  /* ############################################################### */

  val closureCode = new Variable("closure code")
  val closureEnvironment = new Variable("closure environment", isPointer = true)
  /** A chunk representing a closure consisting of:
    * - the address of the code of the closure
    * - the address of the frame of the enclosing environment of the closure, which will become the static link when
    *   the closure is invoked
    */
  lazy val closureChunk = Chunk(Seq(closureCode, closureEnvironment))

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */
  def compilerA6(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters and static link of each procedure. */
    val paramChunks: Map[Procedure, Chunk] = makeMap(procedures, procedure => {
      new Chunk(procedure.staticLink +: procedure.parameters)
      }
    )

    /** The set of procedures whose frame needs to be allocated on the heap (instead of on the stack).
      * This includes:
      * - every procedure that is ever made into a closure
      * - recursively, every enclosing procedure that contains an inner procedure nested within it whose frame is
      *   allocated on the heap
      */
    val frameOnHeap: Set[Procedure] = {
      val out = mutable.Set[Procedure]()

      def helper(p: Procedure): Unit ={
        def f : PartialFunction[Code, Code] = {
          case Closure(c: Procedure) => {
            out.add(c);
            Word(encodeSigned(1))
          }
        }
        transformCode(p.code, f)
      }
      inputProcedures.foreach(helper)

      val out2 = mutable.Set[Procedure]()
      def goUpHelper(procedure: Procedure): Unit ={
        out2.add(procedure)
        procedure.outer match {
          case Some(value) => goUpHelper(value)
          case None => ()
        }
      }
      out.foreach(goUpHelper);

      out2.toSet
    }

    println("frame on heap is", frameOnHeap)

    /** The first phase of compilation: performs the transformations up to eliminateScopes so that
      * the full set of variables of the procedure is known, and so that a `Chunk` can be created for the
      * frame of the procedure. Since the second phase requires a frame for every
      * procedure, the first phase must be completed for all the procedures before the second phase
      * can begin. Returns the code for the procedure and the `Chunk` for the procedure's frame.
      */
    def phaseOne(currentProcedure: Procedure): (Code, Chunk) = {

      /** Generates the code that computes the value for the static link in a call to callee.
        * Specifically, the static link should be the address of the frame in the current chain
        * of static links that corresponds to the procedure that directly encloses callee.
        * This address should be placed in `Reg.result`.
        *
        * When `callee` is a top-level procedure with no outer enclosing procedure, returns code that
        * places the zero word in `Reg.result`.
        */
      def computeStaticLink(callee: Procedure): Code = {
        val caller = currentProcedure
        val outer = callee.outer

        //println(callee.outer + " outer for " + callee.name)
        if(outer.isEmpty){
          //println("Returning  null static link for " + callee.name)
          block(
            //Comment("Static link is ZERO should be outer function!" + callee.name),
            ADD(Reg.result, Reg.zero)
          )
        }else{
          var numberOfStaticLinkJumpsNeeded = caller.depth - callee.depth + 1;
          var numberOfStaticLinkJumpsNeededBackup = caller.depth - callee.depth + 1;
          if(numberOfStaticLinkJumpsNeeded == 0){
            block(
              Comment("first case: static link for " +  callee.name + " from " + caller.name + " is " + caller.name),
              ADD(Reg.result, Reg.framePointer)
            )
          }else{
            var oneUnderEnclosing = currentProcedure;

            while(numberOfStaticLinkJumpsNeeded > 1){
              if(oneUnderEnclosing == null){
                sys.error("Trying to follow static links but given null for outer!")
              }
//              instructions = instructions ++ Seq(
////                oneUnderEnclosing.outer.orNull.
////                block(
////                  Comment("Computing static link jump " + numberOfStaticLinkJumpsNeeded),
////                  // load frame
//////                  paramChunks(currentProcedure).load(Reg.framePointer, Reg.result, frameVar),
////                  read(Reg.result, frameVar)
////                  // load param chunk from the frame
////                  paramChunks(currentProcedure).load(Reg.result, Reg.result, oneUnderEnclosing.paramPtr),
////                  //load static link in the param chunck
////                  paramChunks(oneUnderEnclosing).load(Reg.result, Reg.result, oneUnderEnclosing.staticLink),
////
////                  paramChunks(currentProcedure).store(Reg.framePointer, frameVar, Reg.result)
////                )
//              );
              numberOfStaticLinkJumpsNeeded -= 1
//              if(numberOfStaticLinkJumpsNeeded != 1){
                oneUnderEnclosing = oneUnderEnclosing.outer.orNull;
//              }
            }

            //                    block(paramChunks(currentProcedure).load(Reg.framePointer, Reg.result, frameVar))
        //                    ADD(Reg.result, oneUnderEnclosing.si )
            println(caller, callee, oneUnderEnclosing)
              block(
                    Comment("jumps needed" + numberOfStaticLinkJumpsNeededBackup),
                      Comment(" static link for " +  callee.name + " from " + caller.name + " is " + oneUnderEnclosing.outer.orNull.name),
                      read(Reg.result, oneUnderEnclosing.staticLink)
              )

          }


//          val i = new Variable("static link jump counter")
//          val static = new Variable("static link tmp")
//          Scope( Seq(i, static),
//            block(
//              write(static, currentProcedure.)
//              LIS(Reg.scratch),
//              Word(encodeUnsigned(numberOfStaticLinksFollowed)),
//              whileLoop(read(Reg.result, i), gtCmp, ADD(Reg.result, Reg.zero), block())
//            )
//          )
        }
      }

      /** Eliminate all `Call`s and `CallClosure`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `CallClosure` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = {
        val caller = currentProcedure
        //println(code)
        def fun: PartialFunction[Code, Code] = {
          case Call(procedure, args, isTail) => {
            val isHeapAllocated = frameOnHeap.contains(procedure)
            val currentIsHeapAllocated = frameOnHeap.contains(currentProcedure)
            val caller = currentProcedure
            val callee = procedure
            val tempVars = createTempVars(procedure.parameters).toList
            val tmpStaticLink = new Variable("temp static link")
            val paramChunk = paramChunks.get(procedure) match {
              case Some(value) => value
              case None => sys.error("Param chunk not found for procedure with name " + procedure.name)
            }
            val tmpParamsChunk = new Chunk(tmpStaticLink +: procedure.parameters);

            Scope(tempVars ++ Seq(tmpStaticLink), block(
              Comment("Start of call to " + callee.name + " from " + caller.name),
              Block(args.zipWithIndex.map { case (argCode, i) => {
                block(
                  Comment("Evaluating arg code " + i),
                  argCode,
                  Comment("writing to temp var " + tempVars(i)),
                  write(tempVars(i), Reg.result),
                )
              }
              }),
              computeStaticLink(callee),
              write(tmpStaticLink, Reg.result),
              if (isTail && !currentIsHeapAllocated && currentProcedure.name != "start") {
                //println(" Doing tail call optimization for call from " + currentProcedure.name + " to " + callee.name)
                block(
                  Comment(" Doing tail call optimization for call from " + currentProcedure.name + " to " + callee.name),
                  Stack.allocate(tmpParamsChunk),
                  Block(tmpParamsChunk.variables.zipWithIndex.map { case (paramVar, i) =>
                    if(i == 0){ // i == 0
                      block(
                        read(Reg.scratch, tmpStaticLink),
                        tmpParamsChunk.store(Reg.result, tmpStaticLink, Reg.scratch),
                      )
                    }else{
                      block(
                        read(Reg.scratch, tempVars(i - 1)),
                        tmpParamsChunk.store(Reg.result, paramVar, Reg.scratch)
                      )
                    }
                  }),
                  read(Reg.link, currentProcedure.savedPC),
                  read(Reg.framePointer, currentProcedure.dynamicLink),
                  Stack.pop, // tmp params
                  Stack.pop, // currentProcedure frame
                  Stack.pop, // currentProcedure params
                  ADD(Reg.savedParamPtr, Reg.result), // store tmp param pointer
                  if (isHeapAllocated) heap.allocate(paramChunk) else Stack.allocate(paramChunk),
                  copyChunk(Reg.result, Reg.savedParamPtr),
                  /*
                  Block(tmpParamsChunk.variables.zipWithIndex.map { case (paramVar, i) =>
                    if(i == 0){ // i == 0
                      block(
                        tmpParamsChunk.load(Reg.savedParamPtr, Reg.scratch, tmpStaticLink),
                        paramChunk.store(Reg.result, procedure.staticLink, Reg.scratch),
                      )
                    }else{
                      block(
                        tmpParamsChunk.load(Reg.savedParamPtr, Reg.scratch, procedure.parameters(i-1)),
                        paramChunk.store(Reg.result, paramVar, Reg.scratch)
                      )
                    }
                  }),
                   */
                  LIS(Reg.targetPC),
                  Use(callee.label),
                  JR(Reg.targetPC),
                )
              }
              else{
                block(
                  Comment(" doing normal "),
                  if (isHeapAllocated) heap.allocate(paramChunk) else Stack.allocate(paramChunk),
                  Block(
                    paramChunk.variables.zipWithIndex.map { case (paramVar, i) =>
                      if(paramVar == procedure.staticLink){ // i == 0
                        block(
                          read(Reg.scratch, tmpStaticLink),
                          paramChunk.store(Reg.result, procedure.staticLink, Reg.scratch),
                        )
                      }else{
                        block(
                          read(Reg.scratch, tempVars(i - 1)),
                          paramChunk.store(Reg.result, paramVar, Reg.scratch)
                        )
                      }
                    },
                  ),
                  LIS(Reg.targetPC),
                  Use(callee.label),
                  JALR(Reg.targetPC),
                  )
                }
            ))
          }
          case CallClosure(closure, arguments: Seq[Code], parameters: Seq[Variable], isTail) => {
            val staticLink = new Variable("staticLink", isPointer = true)
            val paramChunk = new Chunk(staticLink +: parameters)
            val tempVars = createTempVars(parameters)
            val closureLocation = new Variable("tmp closure chunk location")
            if(arguments.length != parameters.length){
              sys.error("Invalid closure call! " + arguments.length + " vs " + parameters.length + " args !")
            }
              /*
              Evaluate params to tempary variables
              put params into param frame
              set static link to closure enviroment
              call closure
               */
            Scope(tempVars ++ Seq(staticLink, closureLocation), block(
              Comment("Start of call to closure from " + caller.name),
              Block(arguments.zipWithIndex.map { case (argCode, i) => {
                block(
                  Comment("Evaluating arg code " + i),
                  argCode,
                  Comment("writing to temp var " + tempVars(i)),
                  write(tempVars(i), Reg.result),
                )
              }
              }),
              // Get the static link
              Comment("Before evaluating closure"),
              (closure match {
                case Call(procedure, arguments, isTail) => fun(Call(procedure, arguments, isTail))
                case CallClosure(closure, arguments, parameters, isTail) => fun(CallClosure(closure, arguments, parameters, isTail))
                case any => any
              }),
              Comment("After evaluating closure"),
              write(closureLocation, Reg.result),
              closureChunk.load(Reg.result, Reg.result, closureEnvironment),
              write(staticLink, Reg.result),
              heap.allocate(paramChunk),
              Block(
                paramChunk.variables.zipWithIndex.map { case (paramVar, i) =>
                  if(i == 0){ // is the last one, static link
                    //println(" var should be static link " + paramVar)
                    block(
                      read(Reg.scratch, staticLink),
                      paramChunk.store(Reg.result, staticLink, Reg.scratch),
                    )
                  }else{
                    //println(" should match " +  paramVar + tempVars(i-1))
                    block(
                      read(Reg.scratch, tempVars(i - 1)),
                      paramChunk.store(Reg.result, paramVar, Reg.scratch)
                    )
                  }
                },
              ),
              Comment("about to call closure "),
              read(Reg.targetPC, closureLocation),
              closureChunk.load(Reg.targetPC, Reg.targetPC, closureCode),
              JALR(Reg.targetPC),
            ))
          }
        }


        transformCode(code, fun)
      }

      /** Eliminate all `Closure`s from a tree of `Code` by translating them to simpler pieces of `Code`.
        **/
      def eliminateClosures(code: Code): Code = {
        def f: PartialFunction[Code, Code] = {
          case Closure(procedure) => {
            val closureLocation = new Variable("temp closure location " + procedure.name)
            val staticLink = new Variable("temp temp static link for calling closure " + procedure.name)

            Scope(Seq(closureLocation, staticLink), block(
              LIS(Reg.result),
              Use(procedure.label),
              write(closureLocation, Reg.result),
              computeStaticLink(procedure),
              write(staticLink, Reg.result),
              heap.allocate(closureChunk),
              read(Reg.scratch, closureLocation),
              closureChunk.store(Reg.result, closureCode, Reg.scratch),
              read(Reg.scratch, staticLink),
              closureChunk.store(Reg.result, closureEnvironment, Reg.scratch)
              // returning the closure chunk
            ))
          }
        }

        transformCode(code, f)
      }

      /** Find `Call`s that appear in tail position in their containing procedure. Replace each one with the same
        * `Call` but with `isTail` set to `true`.
        *
        * Hint: If a call in tail position is to a callee procedure nested within the caller procedure, is it safe to do
        * a tail call?
        *
        * As given, this method just returns the `code` unchanged. When you implement handling of tail calls in
        * Assignment 6, you will change the method body to actually detect tail calls.
        */
      def detectTailCalls(code: Code): Code = {
        code match {
          case Block(stmts) => {
            if(stmts.nonEmpty) Block(stmts.dropRight(1) :+ detectTailCalls(stmts.last)) else block()
          }
          case Scope(variables, code) => Scope(variables, detectTailCalls(code))
          case IfStmt(elseLabel, e1, comp, e2, thens, elses) => IfStmt(elseLabel, e1, comp, e2, detectTailCalls(thens), detectTailCalls(elses))
          case Call(procedure, arguments, _) => {
            var isTail = true
            var p = procedure.outer;
            while(p.isDefined){
              if (p.get == currentProcedure){
                isTail = false;
              }
              p = p.get.outer;
            }
            Call(procedure, arguments, isTail = isTail)
          }
          case CallClosure(closure, arguments, parameters, _) => CallClosure(closure, arguments, parameters, isTail = true)
          case x => x
        }
      }

      /* Main body of phaseOne. */

      val code1 = eliminateClosures(currentProcedure.code)
      val code2 = detectTailCalls(code1)
      val code3 = eliminateCalls(code2)
      val code4 = eliminateIfStmts(code3)
      val (code5, variables) = eliminateScopes(code4)
      assert(variables.distinct == variables)

      val frame = Chunk(variables ++ Seq(
        currentProcedure.savedPC,
        currentProcedure.paramPtr,
        currentProcedure.dynamicLink,
      ))

      (code5, frame)
    }

    val phaseOneResults: Map[Procedure, (Code, Chunk)] = makeMap(procedures, phaseOne)

    def phaseTwo(currentProcedure: Procedure): Code = {
      val (code, frame) = phaseOneResults(currentProcedure)
      val isHeapAllocated = frameOnHeap.contains(currentProcedure)

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * Hint: The implementation of this method starts out the same as in Assignment 5, and you can copy
        * your code from there. When you implement closures, you need to modify it so that the frame
        * and parameters are on the heap if `frameOnHeap(currentProcedure)` is true.
        *
        * The prologue assumes that when the procedure is called, `Reg.result` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk to stores it into the `paramPtr` variable in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack or heap
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the callee
        * - saves `Reg.link` in the `savedPC` variable of the `frame`
        *
        * The epilogue:
        * - restores `Reg.link` and `Reg.framePointer` from the current frame
        * - pops the callee's parameter chunk and `frame` off the stack if they are allocated on the stack
        * - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has been called. Therefore, this method
        * should not introduce any new `VarAccess`es into the code (by calling `read` or `write`).
        */
      def addEntryExit(code: Code): Code = {
        val enter = block(
          ADD(Reg.savedParamPtr, Reg.result), // store param ptr until we make a proper frame
          if (isHeapAllocated) heap.allocate(frame) else Stack.allocate(frame),
          frame.store(Reg.result, currentProcedure.savedPC, Reg.link), // store return address
          frame.store(Reg.result, currentProcedure.paramPtr, Reg.savedParamPtr), // store saved param pointer
          frame.store(Reg.result, currentProcedure.dynamicLink, Reg.framePointer), // save callers frame pointer
          ADD(Reg.framePointer, Reg.result), // set the new frame pointer
          Comment("Start of body for " + currentProcedure.name),
        )
        val normalExit = block(
          Comment("End of body for " + currentProcedure.name),
          frame.load(Reg.framePointer, Reg.link, currentProcedure.savedPC), // load return address
          Comment("Restoring older frame pointer in procedure " + currentProcedure.name),
          frame.load(Reg.framePointer, Reg.framePointer, currentProcedure.dynamicLink), // load old frame pointer
          Comment("after restore " + currentProcedure.name),
          if (!isHeapAllocated) block(Stack.pop, Stack.pop, JR(Reg.link)) else JR(Reg.link)
        )

        val tailExit = block(
          Comment(" Doing tail exit for " + currentProcedure.name)

        )
        if(code == null){
          sys.error("No body given for procedure: " + currentProcedure.name)
        }
        block(Define(currentProcedure.label), enter, code,  normalExit)
      }
      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 5, this method handles accesses to variables (and parameters) outside the currently
        * executing procedure, but in one of the outer procedures within which the current procedure is
        * nested. To do this, look up the chain of static links to find the frame (and its associated parameters)
        * of the procedure in which the variable is defined.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value of Reg.scratch.
        * If the access is a read, it may also of course modify the target register. If you need more
        * than these registers, you may add new scratch registers to Reg.scala. The generated code must
        * not modify the values of any other registers that are already listed in Reg.scala.
        *
        * An error that students often make when implementing this method is to assume that the staticLink
        * and paramPtr are at constant offsets from the beginning of a chunk. The offsets change depending
        * on how many variables and parameters a given function has. Do not assume that the offsets are constant.
        * Instead, use `Chunk.read` and `Chunk.write` with the appropriate `Variable`s to access these values
        * in the chunk.
        */
      def eliminateVarAccesses(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case VarAccess(reg, variable, read) => {
//            val currentFrame = new Variable("going up static links - current frame pointer")

            // the frame of the method calling
            val actualFrame = frame

            def getValInProcedure( procedure: Procedure): Code ={
              // assumes that the actualFrame variable has the pointer to the currentFrameObject
              val paramChunk = paramChunks.getOrElse(procedure, sys.error("failed to get param chunk"))
              val currentFrameChunk = phaseOneResults(procedure)._2

              if(currentFrameChunk.variables.contains(variable)){
                //println("Variable " + variable + " is in " + procedure.name + " vars ")
                if(read){
                  block(
//                    actualFrame.load(Reg.framePointer, Reg.result, currentFrame),
//                    ADD(Reg.result, Reg.scratchForStaticLink),
                    // load this variable
                    currentFrameChunk.load(Reg.scratchForStaticLink, reg, variable),
                  )
                }else{
                  block(
//                    actualFrame.load(Reg.framePointer, Reg.result, currentFrame),
//                    ADD(Reg.result, Reg.scratchForStaticLink),
                    // load this variable
                    currentFrameChunk.store(Reg.scratchForStaticLink,  variable, reg),
                  )
                }
              }else if(paramChunk.variables.contains(variable)){
                //println("Variable " + variable + " is in " + procedure.name + " params ")
                if(read){
                  block(
//                    actualFrame.load(Reg.framePointer, Reg.result, currentFrame),
//                    ADD(Reg.result, Reg.scratchForStaticLink),
                    currentFrameChunk.load(Reg.scratchForStaticLink, Reg.scratch, procedure.paramPtr),

                    // load this variable
                    paramChunk.load(Reg.scratch, reg, variable),
                  )
                }else{
                  block(
//                    actualFrame.load(Reg.framePointer, Reg.result, currentFrame),
                    currentFrameChunk.load(Reg.scratchForStaticLink, Reg.scratch, procedure.paramPtr),
                    paramChunk.store(Reg.scratch, variable, reg),
                  )
                }
              }
                // have to follow static links
              else{
                println(procedure.outer)
                block(
//                  actualFrame.load(Reg.framePointer, Reg.result, currentFrame),
                  currentFrameChunk.load(Reg.scratchForStaticLink, Reg.scratch, procedure.paramPtr),
                  paramChunk.load(Reg.scratch, Reg.scratchForStaticLink, procedure.staticLink),
//                  actualFrame.store(Reg.framePointer, currentFrame, Reg.result),
//                  ADD(Reg.scratchForStaticLink, Reg.),
                  getValInProcedure(procedure.outer.getOrElse(sys.error("Could not find variable anywhere!! : " + variable))),
                )
              }
            }

             block(
//              actualFrame.store(Reg.framePointer, currentFrame, Reg.framePointer),
               Comment("Doing var access " + variable +" "+ currentProcedure.name),
               ADD(Reg.scratchForStaticLink, Reg.framePointer),
                getValInProcedure(currentProcedure)
            )
          }
        }

        transformCode(code, fun)
      }

      /* Main body of phaseTwo. */

      val code1 = eliminateVarAccesses(code)
      val code2 = addEntryExit(code1)
      code2
    }

    /* Main body of compilerA6. */

    val code = block(
      heap.initCode,
      Stack.allocate(Chunk(procedures.head.parameters :+ procedures.head.staticLink)), // allocate parameter chunk for start procedure
      Block(procedures.map(phaseTwo))
    )
    toMachineCode(code)
  }

}

