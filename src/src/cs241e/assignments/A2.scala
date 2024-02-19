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

import Assembler._
import ProgramRepresentation._
import Transformations._
import cs241e.mips._

object A2 {
  /* As part of Assignment 2, before implementing the methods in this file, first implement the methods in
   * Transformations.scala in the section for Assignment 2.
   */

  /* Registers 1 and 2 hold 32-bit integers (in two's-complement notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  lazy val maximum: Seq[Word] = {
    val endlabel = new Label("end")
    val oneIsSmaller = new Label("oneIsSmaller")

//
//    val code = Seq[Code](
//      SLT(Reg(3), Reg(1), Reg(2)),
//      LIS(Reg(4)),
//      Word(encodeUnsigned(1)),
//      beq(Reg(3), Reg(4), endlabel),
//      BEQ(Reg(3), Reg(4), 1),
//      Define(endlabel),
//      JR(Reg(31))
//    )

    val code = Seq[Code](
      SLT(Reg(3), Reg(1), Reg(2)), // if 1 < 2, then reg 3 = 1
      LIS(Reg(4)),
      Word(encodeUnsigned(1)),
      beq(Reg(3), Reg(4), oneIsSmaller),
      // reg 1 is bigger
      ADD(Reg(3), Reg(1), Reg.zero),
      LIS(Reg(5)),
      Use(endlabel),
      JR(Reg(5)),
      Define(oneIsSmaller),
      // reg 2 is bigger
      ADD(Reg(3), Reg(2), Reg.zero),
      Define(endlabel),
      JR(Reg(31)),
    )
    eliminateLabels(code)
  }

  /* Registers 1 and 2 hold 32-bit integers (in unsigned integer notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  lazy val maximumUnsigned: Seq[Word] = {
    val endlabel = new Label("end")
    val oneIsSmaller = new Label("oneIsSmaller")

    val code = Seq[Code](
      SLTU(Reg(3), Reg(1), Reg(2)), // if 1 < 2, then reg 3 = 1
      LIS(Reg(4)),
      Word(encodeUnsigned(1)),
      beq(Reg(3), Reg(4), oneIsSmaller),
      // reg 1 is bigger
      ADD(Reg(3), Reg(1), Reg.zero),
      LIS(Reg(5)),
      Use(endlabel),
      JR(Reg(5)),
      Define(oneIsSmaller),
      // reg 2 is bigger
      ADD(Reg(3), Reg(2), Reg.zero),
      Define(endlabel),
      JR(Reg(31)),
    )
    eliminateLabels(code)
  }

}

