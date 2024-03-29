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

import cs241e.mips._
import Assembler._
import Transformations._
import ProgramRepresentation._
import CodeBuilders._
import A1._
import cs241e.assignments

object A4 {
  /** This is an enhanced version of the loadAndRun method from Assignment 1.
    *
    * It loads the give machine language code into memory, writes the specified values to registers 1 and 2,
    * and then runs the CPU on the code that was loaded. The debugger can be invoked by passing the argument
    * debug = true.
    */
  def loadAndRun(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero, debug: Boolean = false): State = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    if(debug) Debugger.debug(initialState, code.debugTable)
    else CPU.run(initialState)
  }

  /** A utility method that takes two sequences of words representing `code` and an `array`,
    * loads both into memory, and sets register 1 to the address of the beginning
    * of the array and register 2 to its length in words.
    */
  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }

  /** A utility method that loads code and an array into memory and runs the code.
    * The debugger can be invoked by passing the argument debug = true.
    */
  def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
    val state = loadCodeAndArray(code.words, array)
    if(debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers.
    * Register 2 holds the number of elements in the array.
    * If the array is empty place the value -1 in register 3.
    * Otherwise copy the last element of the array into register 3.
    */
  lazy val lastElement: MachineCode = {
    val code: Code = block(
      ifStmt(ADD(Reg.result, Reg(2)), neCmp, ADD(Reg.result, Reg.zero),
        block(
          deref(
            binOp(ADD(Reg.result, Reg(1)), plus,
              binOp(
                binOp(
                  ADD(Reg.result, Reg(2)),
                  minus,
                  block(
                    LIS(Reg.result),
                    Word(encodeUnsigned(1)),
                  )
                ),
                times,
                block(
                  LIS(Reg.result),
                  Word(encodeUnsigned(4)),
                )
              )
            )
          )
        ),
        // Else
        block(
          LIS(Reg.result),
          Word(encodeSigned(-1))
        )
      ),
    )
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit two's-complement integers.
    * Register 2 holds the number of elements in the array.
    * Determine the maximum of all the elements of the array, write it into register 3.
    * Assume the array is not empty.
    */
  lazy val arrayMaximum: MachineCode = {
    val i = new Variable("i")
    val max = new Variable("max")
    val currEl = new Variable("currEl")
    val code: Code = Scope(Seq(i, max, currEl),
      block(
        write(i, Reg.zero),
        whileLoop(read(Reg.result, i), ltCmp, ADD(Reg.result, Reg(2)), block(
          //Reg(1) + i * 4
          deref(
            binOp(ADD(Reg.result, Reg(1)), plus,
              binOp(read(Reg.result, i), times,
                block(LIS(Reg.result), Word(encodeUnsigned(4)))
            ))
          ),
          write(currEl, Reg.result),
          ifStmt(read(Reg.result, currEl), gtCmp, read(Reg.result, max), block(
            assign(max, read(Reg.result, currEl)),
          )),

          // i++
          binOp(read(Reg.result, i), plus, block(LIS(Reg.result), Word(encodeUnsigned(1)))),
          write(i, Reg.result),
        )),
        read(Reg.result, max)
      )
    )
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers, each representing a character.
    * The integer zero represents a space, and each integer i (1 <= i <= 26) represents the i'th letter of the
    * uppercase alphabet.
    * Register 2 holds the number of elements in the array (can be empty).
    * Your program should output the uppercase characters represented by the integers in the array.
    * The MIPS system allows you to output one character at a time, by storing its ASCII value into the
    * special memory location CPU.printAddr (1111 1111 1111 1111 0000 0000 0000 1100).
    *
    * Hint: use Google to find an ASCII code table.
    */
  
  lazy val outputLetters: MachineCode = {
    val i = new Variable("i")
    val max = new Variable("max")
    val currEl = new Variable("currEl")

    val code: Code = Scope(Seq(i, currEl), block(
      write(i, Reg.zero),


      whileLoop(readVarRes(i), ltCmp, ADD(Reg.result, Reg(2)), block(
          Comment("Doing while loop iteration"),
          deref(binOp(ADD(Reg.result, Reg(1)), plus, binOp(readVarRes(i), times, constRes(4)))),
          writeVarRes(currEl),
          ifStmt(readVarRes(currEl), eqCmp, constRes(0), block(
              // is zero, print space
              constRes(32), // 32 is space
              LIS(Reg.scratch),
              Word("1111 1111 1111 1111 0000 0000 0000 1100"),
              SW(Reg.result, 0, Reg.scratch),
            ),
            block(
              // no zero
              binOp(readVarRes(currEl), plus, constRes(64)),
              LIS(Reg.scratch),
              Word("1111 1111 1111 1111 0000 0000 0000 1100"),
              SW(Reg.result, 0, Reg.scratch),
            )
          ),
          binOp(readVarRes(i), plus, constRes(1)),
          writeVarRes(i),
          read(Reg(19), i),
        )
      )
    ))
    compilerA4(code)
  }

  /** Register 1 holds a 32-bit integer (in two's-complement notation).
    * Your program should format thisencodeSignedinteger in base 10, print it, then print a newline character.
    */

  lazy val printIntegerCode: Code = {
    val stackStart = new Variable("stack-start")
    val stackCurrent = new Variable("stack-current")
    val endLabel = new Label("end Label")
    val printLabel = new Label("print val")
    val stackEnd = new Label(" stack end")

     Scope(Seq(stackStart, stackCurrent), block(
       LIS(Reg.result),
       Use(endLabel),
       write(stackStart, Reg.result),
       assign(stackCurrent, readVarRes(stackStart)),

       // if is -2147483648, we hard code, since algorithm involves
       // multiplying by *-1, which will fail since  2147483648 doesnt fit

       ifStmt(ADD(Reg.result, Reg(1)), eqCmp, constRes(-2147483648), block(
         // print -
         LIS(Reg.scratch),
         Word("1111 1111 1111 1111 0000 0000 0000 1100"),
         LIS(Reg.result),
         Word(encodeUnsigned(45)), //  48 is - in ASCI
         SW(Reg.result, 0, Reg.scratch),

         // put number in stack
         binOp(readVarRes(stackCurrent), plus, constRes(4)),
         write(stackCurrent, Reg.result),
         read(Reg.scratch, stackCurrent),

         LIS(Reg.result),
         Word(encodeSigned(48 + 8)),
         SW(Reg.result, 0, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 4)),
         SW(Reg.result, 4, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 6)),
         SW(Reg.result, 8, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 3)),
         SW(Reg.result, 12, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 8)),
         SW(Reg.result, 16, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 4)),
         SW(Reg.result, 20, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 7)),
         SW(Reg.result, 24, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 4)),
         SW(Reg.result, 28, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 1)),
         SW(Reg.result, 32, Reg.scratch),

         LIS(Reg.result),
         Word(encodeSigned(48 + 2)),
         SW(Reg.result, 36, Reg.scratch),


         // put number in stack
         binOp(readVarRes(stackCurrent), plus, constRes(36)), // 8 * 4
         write(stackCurrent, Reg.result),
         read(Reg.scratch, stackCurrent),

         beq(Reg.zero, Reg.zero, printLabel)
       )),



       // if less than 1
       ifStmt(ADD(Reg.result, Reg(1)), ltCmp, ADD(Reg.result, Reg.zero),
         block(
           Comment("Times by -1"),
           binOp(ADD(Reg.result, Reg(1)), times, constRes(-1)),
           ADD(Reg(1), Reg.result),


           // print -
           LIS(Reg.scratch),
           Word("1111 1111 1111 1111 0000 0000 0000 1100"),

           LIS(Reg.result),
           Word(encodeUnsigned(45)), //  48 is - in ASCI
           SW(Reg.result, 0, Reg.scratch),
         )
       ),
       // if 0
      ifStmt(ADD(Reg.result, Reg(1)), eqCmp, ADD(Reg.result, Reg.zero), block(
        LIS(Reg.scratch),
        Word("1111 1111 1111 1111 0000 0000 0000 1100"),

        LIS(Reg.result),
        Word(encodeUnsigned(48)), //  48 is 0 in ASCI
        SW(Reg.result, 0, Reg.scratch),
      )),

      whileLoop(ADD(Reg.result, Reg(1)), neCmp, ADD(Reg.result, Reg.zero), block(
        // increment stack
        binOp(readVarRes(stackCurrent), plus, constRes(4)),
        write(stackCurrent, Reg.result),

        Comment("get remainder"),
        binOp(binOp(ADD(Reg.result, Reg(1)), remainder, constRes(10)), plus, constRes(48)),

        read(Reg.scratch, stackCurrent),
        SW(Reg.result, 0, Reg.scratch), // store remainder in stack current

        Comment("Divide by 10"),
        binOp(ADD(Reg.result, Reg(1)), divide, constRes(10)),
        ADD(Reg(1), Reg.result),
      )),


       Define(printLabel),
       whileLoop(readVarRes(stackCurrent), neCmp, readVarRes(stackStart), block(
         Comment("print from stack"),
         readVarRes(stackCurrent),
         LW(Reg.result, 0, Reg.result), // read from stack

         LIS (Reg.scratch),
         Word("1111 1111 1111 1111 0000 0000 0000 1100"),

         SW(Reg.result, 0, Reg.scratch),

         // Decrement stack
         binOp(readVarRes(stackCurrent), minus, constRes(4)),
         write(stackCurrent, Reg.result)
       )),

      Comment("Print New line"),
      LIS(Reg.scratch),
      Word("1111 1111 1111 1111 0000 0000 0000 1100"),
      LIS(Reg.result),
      Word(encodeUnsigned(10)), //  10 is \n in ASCI
      SW(Reg.result, 0, Reg.scratch),
       LIS(Reg.result),
      Use(stackEnd),
       JR(Reg.result),
       Define(endLabel),
       // Reserve some space on the stack for printing
     Block(Seq.range(0, 200).map(x => Word(encodeUnsigned(0)))),
       Define(stackEnd),
    ))
  }
  lazy val printInteger: MachineCode = compilerA4(printIntegerCode)
}
