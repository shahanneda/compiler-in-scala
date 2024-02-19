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
import cs241e.assignments.Assembler.{ADD, LIS, LW, encodeSigned}
import cs241e.mips.Word

object A5 {
  lazy val loop: Procedure = {
    val arg1 = new Variable("arg 1 of procedure")
    val procedure = new Procedure("loopProcedure", Seq(arg1))
    procedure.code = block(
      ifStmt(read(Reg.result, arg1), ltCmp, constRes(100), block(
        Call(procedure, Seq(binOp(
          read(Reg.result, arg1),
          plus,
          constRes(1)
        ))
        )
      ),
        // else
        read(Reg.result, arg1),
      )
    )
    procedure
  }

  val procedure2 = new Procedure("other", Seq())

  lazy val twoProTest: Procedure = {
    val arg1 = new Variable("arg 1 of procedure")

    val testinnervar = new Variable("tt")
    val testinnervar2 = new Variable("tt")
    val testinnervar3 = new Variable("tt")
    procedure2.code = Scope(Seq(testinnervar, testinnervar2, testinnervar3), block())


    val varb = new Variable("test variable")

    val procedure = new Procedure("primary", Seq(arg1))
    procedure.code = Scope(Seq(varb), block(
      Comment("loading into reg 3"),
      read(Reg.result, arg1),
      write(varb, Reg.result),
      read(Reg.result, varb),

      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(printProcedure, read(Reg.result, varb)),
      call(procedure2),
      call(procedure2),

      read(Reg.result, varb),
    ))
    procedure
  }

  /** The code of `printInteger` from Assignment 4 encapsulated as a `Procedure`. The procedure should have
    * exactly one parameter, the integer to be printed. */
  lazy val printProcedure: Procedure = {
    val arg1 = new Variable("arg 1 of procedure")
    val procedure = new Procedure("printProcedure", Seq(arg1))
    procedure.code = block(
      read(Reg(1), arg1),
      A4.printIntegerCode,
    )
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    * The procedure should call `printProcedure` for each integer in the array in turn,
    * to print all the integers, and return.
    *
    * Test this procedure by compiling it with `printProcedure` and running it on various arrays.
    */
  lazy val printArray: Procedure = {
    val arrAddress = new Variable("array address")
    val arrayLen = new Variable("array length")
    val procedure = new Procedure("printArray", Seq(arrAddress, arrayLen))
    val i = new Variable("i")
    procedure.code = block(
      Scope(Seq(i),
        block(
          whileLoop(read(Reg.result, i), ltCmp, read(Reg.result, arrayLen), block(
            // arrAddres + i * 4
            Comment("Array address in register 3:"),
            read(Reg.result, arrAddress),
            binOp(read(Reg.result, arrAddress), plus, binOp(read(Reg.result, i), times, constRes(4))),
            Comment("Finished calculating new index, now loading the result from memory (index in reg 3)"),
            LW(Reg.result, 0, Reg.result),

            call(printProcedure, ADD(Reg.result, Reg.result)),

            binOp(read(Reg.result, i), plus, constRes(1)),
            write(i, Reg.result),
        )
      )
    )))
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    *
    * You may use multiple procedures if you wish. Generate them and return them in a `Seq`.
    * The tests will execute the first procedure in the sequence.
    *
    * The task is to determine the height of a binary tree and return it (in `Reg.result`).
    * Assume that every tree contains at least one node and hence has a height of at least one.
    * Each node of the tree is encoded in three consecutive elements (words) of the array:
    * a two's-complement integer stored at the node, the node's left child, and the node's right child.
    * Each child is specified as the array index of the first element of the child node.
    * The integer -1 indicates that a node does not have a left or right child. For example, the following tree:
    *
    *   77
    *  /  \
    * 22    -8
    *     /  \
    *   -36   999
    *
    * could be encoded by following array:
    *
    * A[0] = 77
    * A[1] = 3
    * A[2] = 6
    * A[3] = 22
    * A[4] = -1
    * A[5] = -1
    * A[6] = -8
    * A[7] = 9
    * A[8] = 12
    * A[9] = -36
    * A[10] = -1
    * A[11] = -1
    * A[12] = 999
    * A[13] = -1
    * A[14] = -1
    *
    * in which the root is encoded by the elements A[0], A[1] and A[2], the root's left child is encoded
    * by the elements A[3], A[4] and A[5], the root's right child is encoded by the elements A[6], A[7] and A[8],
    * the root's left-most grandchild is encoded by the elements A[9], A[10] and A[11],
    * and the root's right-most grandchild is encoded by the elements A[12], A[13] and A[14].
    *
    * This example tree has height 3.
    */
  val getAtIndex = {
    val index = new Variable("index")
    val address = new Variable("address")

    val proc = new Procedure("getAtIndex", Seq(index, address))
    proc.code = block(
      binOp(read(Reg.result, address), plus, binOp(read(Reg.result, index), times, constRes(4))),
      LW(Reg.result, 0, Reg.result), // read value
    )
    proc
  }
  lazy val treeHeight: Seq[Procedure] = {
    val array = new Variable("array")
    val length = new Variable("length")
    val treeHeight = new Procedure("treeHeight", Seq(array, length))

    val index = new Variable("index")
    val arrStart = new Variable("array start")
    val currentValue = new Variable("value")
    val leftHeight = new Variable("left height")
    val rightHeight = new Variable("right height")

    val treeHeightIndex = new Procedure("treeHeightIndex", Seq(index, arrStart))



    treeHeightIndex.code = Scope( Seq(currentValue, leftHeight, rightHeight),  block(
      ifStmt(read(Reg.result, index), eqCmp, constRes(-1),
        block(
          LIS(Reg.result),
          Word(encodeSigned(0))
        ), block(
          // get value of current index
          call(getAtIndex, read(Reg.result, index), read(Reg.result, arrStart)),
          write(currentValue, Reg.result),
//          call(printProcedure, read(Reg.result, currentValue)),
          ifStmt(read(Reg.result, currentValue), eqCmp, constRes(-1),
            block(
              LIS(Reg.result),
              Word(encodeSigned(0))
            ),
            block(
              call(treeHeightIndex, call(getAtIndex, binOp(readVarRes(index), plus, constRes(1)),  readVarRes(arrStart)), readVarRes(arrStart)),
              write(leftHeight, Reg.result) ,
              call(treeHeightIndex, call(getAtIndex, binOp(readVarRes(index), plus, constRes(2)),  readVarRes(arrStart)), readVarRes(arrStart)),
              write(rightHeight, Reg.result),

              ifStmt(readVarRes(leftHeight), gtCmp, readVarRes(rightHeight),
                block(
                  binOp(readVarRes(leftHeight), plus, constRes(1)),
                ),
                block(
                  binOp(readVarRes(rightHeight), plus, constRes(1)) ,
                )
              )
            ))
        )
      ),
    ))

    treeHeight.code = block(
      call(treeHeightIndex, constRes(0), read(Reg.result, array))
    )
    /* You may, but are not required to, define and use more procedures in addition to `treeHeight`.
     * `treeHeight` must be the first procedure in the sequence that is returned.
     */
    Seq(treeHeight, getAtIndex, treeHeightIndex, printProcedure)
  }
}
