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
import Assembler._
import cs241e.mips._

/** Methods that generate `Code` for various higher-level language features. */

object CodeBuilders { 
  /* ## Assignment 4 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Generates a binary operation that evaluates `e1` and `e2`, ensures that the `Reg.result` from
    * `e1` is in `Reg.scratch` and that the `Reg.result` from `e2` is still in `Reg.result`,
    * then executes `op`.
    *
    * Hint: Use a temporary variable and a `Scope`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def binOp(e1: Code, op: Code, e2: Code): Code = {
      val e1Result = new Variable("e1 result ")

      Scope(variables = Seq(e1Result), code =
        block(
          e1,
          write(e1Result, Reg.result),
          e2,
          read(Reg.scratch, e1Result),
          // e1 in reg.scratch, e2 in reg.result
          op
        )
      )
  }

  /* The following `Code`s are intended to be used as the `op` argument to `binOp`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, compute the corresponding arithmetic
   * operation, and leave the result of the operation in `Reg.result`.
   *
   * Assume that the operands are to be interpreted as signed two's-complement integers except in
   * `divideUnsigned` and `remainderUnsigned`.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  lazy val plus: Code = ADD(Reg.result, Reg.scratch, Reg.result)
  lazy val minus: Code = SUB(Reg.result, Reg.scratch, Reg.result)
  lazy val times: Code = block(
    MULT(Reg.scratch, Reg.result),
    MFLO(Reg.result)
  )
  lazy val divide: Code = block(
    DIV(Reg.scratch, Reg.result),
    MFLO(Reg.result),
  )
  lazy val remainder: Code = block(
    DIV(Reg.scratch, Reg.result),
    MFHI(Reg.result),
  )

  lazy val divideUnsigned: Code = block(
    DIVU(Reg.scratch, Reg.result),
    MFLO(Reg.result),
  )
  lazy val remainderUnsigned: Code = block(
    DIVU(Reg.scratch, Reg.result),
    MFLO(Reg.result),
  )

  /* The following `Code`s are intended to be used as the `comp` argument to `IfStmt`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, interpret them as two's-complement
   * signed integers (except `gtUnsignedCmp`), compare them, and branch to `label` if the comparison fails.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  def eqCmp(label: Label): Code = block(
    bne(Reg.scratch, Reg.result, label)
  )

  def neCmp(label: Label): Code = block(
    beq(Reg.scratch, Reg.result, label)
  )
  def ltCmp(label: Label): Code = block(
    beq(Reg.scratch, Reg.result,  label),

    SLT(Reg.scratch, Reg.scratch, Reg.result),
    // Scratch will be 1 if s is less than r
      LIS(Reg.result),
      Word(encodeUnsigned(1)),

    // if not 1, then failed
    bne(Reg.scratch, Reg.result, label)
  )

  def gtCmp(label: Label): Code = block(
    /*
     goal s > r
     if s <= r, then branch
     s < r
     */

    beq(Reg.scratch, Reg.result, label),
    SLT(Reg.scratch, Reg.scratch, Reg.result),
    // Scratch will be 1 if s is less than r
    LIS(Reg.result),
    Word(encodeUnsigned(1)),
    // if it is 1, then fail
    beq(Reg.scratch, Reg.result, label)
  )

  def leCmp(label: Label): Code = block(
    /*
     Goal s <= r
        If s > r, then jump
        so if r < s, then 1 so jump
     */
    SLT(Reg.scratch, Reg.result, Reg.scratch),
    LIS(Reg.result),
    Word(encodeUnsigned(1)),
    beq(Reg.result, Reg.scratch, label)
  )

  def geCmp(label: Label): Code = block(
    /*
    Goal s >= r

    fail if s < r,
     */
    SLT(Reg.scratch, Reg.scratch, Reg.result),
    LIS(Reg.result),
    Word(encodeUnsigned(1)),
    beq(Reg.result, Reg.scratch, label)
  )
  def gtUnsignedCmp(label: Label): Code = block(
    /*
        Goal s > r,
        so jump if s <= r,
     */
    beq(Reg.scratch, Reg.result, label),

    // jump if s < r
    SLTU(Reg.scratch, Reg.scratch, Reg.result),
    LIS(Reg.result),
    Word(encodeUnsigned(1)),
    beq(Reg.result, Reg.scratch, label)
  )

  /** Generates code that evaluates `expr` to yield a memory address, then loads the word from that address
    * into `Reg.result`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    **/
  def deref(expr: Code): Code = block(
    expr,
    LW(Reg.result,  0, Reg.result),
  )

  /** Generates code that evaluates `target` to yield a memory address, then evaluates `expr` to yield a value,
    * then stores the value into the memory address.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def assignToAddr(target: Code, expr: Code): Code = {
    val temp = new Variable("Assign to address temp")
    Scope(Seq(temp), block(
      target,
      write(temp, Reg.result),
      expr,
      read(Reg.scratch, temp),
      SW(Reg.result, 0, Reg.scratch)
    ))
  }


  /** Generates code that implements a while loop. The generated code should evaluate `e1` and `e2`,
    * compare them using `comp`, and if the comparison succeeds, it should execute `body` and repeat
    * the entire process from the beginning.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def whileLoop(e1: Code, comp: Label=>Code, e2: Code, body: Code): Code = {
    val temp = new Variable("E1 Result (While)");
    val startLabel = new Label("While Start Label")
    val endLabel = new Label("While End Label")

    Scope(Seq(temp),
      block(
        Define(startLabel),
        e1,
        write(temp, Reg.result),
        e2,
        read(Reg.scratch, temp),
        comp(endLabel),
        body,
        beq(Reg.zero, Reg.zero, startLabel),
        Define(endLabel),
      )
    )
  }

  
}
