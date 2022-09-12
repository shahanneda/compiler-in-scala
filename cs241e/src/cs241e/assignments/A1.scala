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
/* ## Assignment 1 */

package cs241e.assignments

import cs241e.assignments.Assembler._
import cs241e.mips._

object A1 {
  /* As part of Assignment 1, before implementing the methods in this file, first implement the first four methods
   * in Assembler.scala.
   */

  /** The `setMem` method in `State` loads one word into a specific memory location identified by
    * a given address. Implement this extended `setMem` method that takes a sequence of `words` and writes them
    * into successive memory locations in `inputState` starting with a given `startingAddress`. Return the
    * resulting state.
    */
  def setMem(words: Seq[Word], inputState: State = State(), startingAddress: Word = Word.zero): State = {
    require(decodeUnsigned(startingAddress) + words.size*4 <= decodeUnsigned(CPU.maxAddr))

//    println("setting this to memeory:", words)
    var currentAddress = startingAddress
    var state = inputState
    words.foreach(word => {
      state = state.setMem(currentAddress, word)
      currentAddress =  Word(encodeUnsigned( decodeUnsigned(currentAddress) + 4))
    })
//    inputState.setMem(Word.zero, Word(encodeUnsigned(234)))
//    println("memroy is this", inputState.mem(Word.zero))
    state
  }

  /** You can use this helper method to test the following programs that you will write. It loads the
    * given `words` into memory, writes the specified values to registers 1 and 2, and then runs
    * the CPU on the `words` that were loaded.
    */
  def loadAndRun(words: Seq[Word], register1: Word = Word.zero, register2: Word = Word.zero): State = {
    val initialState =
      setMem(words)
        .setReg(1, register1)
        .setReg(2, register2)

//    println(initialState.mem(Word.zero))
//    println(initialState)
//    println(words)
//    println(initialState.mem(Word(encodeUnsigned(0))))
//    println(initialState.mem(Word(encodeUnsigned(4))))
//    println(initialState.mem(Word(encodeUnsigned(8))))
//    println(initialState.mem(Word(encodeUnsigned(12))))
//    println(initialState.mem(Word(encodeUnsigned(16))))

    CPU.run(initialState)
  }

  /** Write a MIPS machine language program that transfers control to the address saved in register 31. This is the only
    * thing that your program should do.
    *
    * Hint: You can create a `Word` of 32 bits as follows: `Word("01010101010101010101010101010101")`.
    */
  lazy val transferTo31 = Seq[Word](
    Word("00000011111000000000000000001000")
  )

  /** Write a MIPS machine language program that copies the value in register 1 to register 3, then adds the values
    * in register 1 and 3, placing the result in register 4, and then ends execution by transferring control to the
    * address in register 31.
    */
  lazy val add134 = Seq[Word](
    Word("000000 00001 00000 00011 000 0010 0000"),
    Word("000000 00001 00011 00100 000 0010 0000"),
    Word("000000 11111 0 0000 0000 0000 0000 1000")
  )

  /* Now implement the code generation methods in the second half of `Assembler.scala`. Then continue here
   * with the following methods.
   */

  /** Write a MIPS machine language program that determines the maximum of the values in registers 1 and 2
    * interpreted as two's-complement integers, places it in register 3, and ends execution.
    */
  lazy val maximum = Seq[Word](
    SLT(Reg(3), Reg(1), Reg(2)),
    // if 1 , then reg 2 is max, else reg 1 is maximum
    LIS(Reg(4)),
    Word(encodeUnsigned(1)),
    LIS(Reg(5)),
    Word(encodeUnsigned(1)),
    BEQ(Reg(3), Reg(4), 2),  // will skip next line if reg 2 is max
    ADD(Reg(3), Reg(1), Reg(0)),
    BEQ(Reg(4), Reg(5), 1),
    ADD(Reg(3), Reg(2), Reg(0)),
    JR(Reg(31))
  )

  /** Write a MIPS machine language program that adds 1 to the value in register 1, places the result in register 3,
    * and then ends execution.
    */
  lazy val addOne = Seq[Word](
    LIS(Reg(2)), // load one into register 2
    Word(encodeUnsigned(1)),

    ADD(Reg(3), Reg(1), Reg(2)),
    JR(Reg(31))
  )

  /** Write a MIPS machine language program that interprets the value in register 1 as the address of a word
    * in memory, places the address of the following word in memory in register 3, and then ends execution.
     */
  lazy val followingAddress = Seq[Word](
    ADD(Reg(3), Reg(1), Reg(0)),
    JR(Reg(31)),
  )
}

