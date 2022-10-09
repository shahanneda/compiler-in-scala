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
    ???
    val procedure = new Procedure("printArray", ???)
    procedure.code = ???
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
  lazy val treeHeight: Seq[Procedure] = {
    val array = new Variable("array")
    val length = new Variable("length")
    val treeHeight = new Procedure("treeHeight", Seq(array, length))
    
    treeHeight.code = ???
    /* You may, but are not required to, define and use more procedures in addition to `treeHeight`.
     * `treeHeight` must be the first procedure in the sequence that is returned.
     */
    Seq(treeHeight, ??? )
  }
}
