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

import cs241e.mips.{Bits, _}
import cs241e.Utils._

import scala.annotation.tailrec

/** An assembler that generates machine language words representing MIPS instructions. */

object Assembler {

  /* ## Assignment 1 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Given a sequence of bits, interpret it as an unsigned binary number and return the number.
    n*
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The purpose of this assignment is for *you* to write code that encodes/decodes numbers in binary.
    * Do not submit solutions that just call functions in the Java/Scala standard library to do the
    * conversion for you.
    **/
  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 0)
    require(bits.length <= 32)

    var power: Long = 1
    bits.foldRight[Long](0)((bit, inc: Long) => {
      val returnVal = (if (!bit) 0 else power) + inc
      power *= 2
      returnVal
    })
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary number and return the number. */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 1)
    require(bits.length <= 32)

    var power: Long = 1
    var counter  = 0
    bits.foldRight[Long](0)((bit, inc) => {
      counter += 1
      if(counter == bits.length){
        power *= -1
      }
      val returnVal = (if (!bit) 0 else power) + inc
      power *= 2
      returnVal
    })
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number using the number of bits
    * specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of bits. When calling this method, one
    * can specify the number of bits explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified
    * (e.g. `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 0)
    require(bits <= 32)
    require(i >= 0)
    require(i < twoTo(bits))

    var value = Seq[Boolean]()
    var num: Long = i
    for(a <- 1 to bits){
      value = value :+ (num % 2 == 1)
      num /= 2
    }
    value.reverse
  }

  /** Given a number `i`, encode it as a signed two's-complement binary number using the number of bits
    * specified by `bits`.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 1)
    require(bits <= 32)
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))

    if (i >= 0){
      return encodeUnsigned(i, bits)
    }

    val number: Long = i * -1
    add1(encodeUnsigned(number, bits).map((a) => !a))
  }

  def add1(seq: Seq[Boolean]): Seq[Boolean] ={
    var carry = true
    seq.reverseIterator.map((a) => {
      val bit = a != carry // XOR
      carry = a && carry
      bit
    }).toSeq.reverse
  }


  /* Before continuing Assignment 1, go to `A1.scala` and complete the methods there. Then return here and implement
   * the following.
   */

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */

  val sixZero: Seq[Boolean] = Bits("000000")

  def regToBinary(r: Reg): Seq[Boolean] = encodeUnsigned(r.number, 5)

  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = Word( sixZero ++ regToBinary(s) ++ regToBinary(t) ++ regToBinary(d) ++ Bits("000 0010 0000"))
  def SUB(d: Reg, s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ regToBinary(d) ++ Bits("00000100010"))
  def MULT(s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ Bits("0000000000011000"))
  def MULTU(s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ Bits("0000000000011001"))
  def DIV(s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ Bits(" 0000000000011010"))
  def DIVU(s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ Bits("0000000000011011"))
  def MFHI(d: Reg): Word = Word(Bits("0000000000000000") ++ regToBinary(d) ++ Bits("00000010000"))
  def MFLO(d: Reg): Word = Word(Bits("0000000000000000") ++ regToBinary(d) ++ Bits("00000010010"))
  def LIS(d: Reg): Word =  Word(Bits("0000 0000 0000 0000") ++ regToBinary(d) ++ Bits("000 0001 0100"))
  def LW(t: Reg, i: Int, s: Reg): Word = Word(Bits("100011") ++ regToBinary(s) ++regToBinary(t) ++ encodeSigned(i, 16))
  def SW(t: Reg, i: Int, s: Reg): Word = Word(Bits("101011") ++ regToBinary(s) ++ regToBinary(t) ++ encodeSigned(i, 16))
  def SLT(d: Reg, s: Reg, t: Reg): Word = {
//    println(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ regToBinary(d) ++ Bits("00000101010"))
    val rval = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ regToBinary(d) ++ Bits("00000101010"))
    rval
  }
  def SLTU(d: Reg, s: Reg, t: Reg): Word = Word(sixZero ++ regToBinary(s) ++ regToBinary(t) ++ regToBinary(d) ++ Bits("00000101011"))
  def BEQ(s: Reg, t: Reg, i: Int): Word = Word(Bits("000100") ++ regToBinary(s) ++ regToBinary(t) ++ encodeSigned(i, 16) )
  def BNE(s: Reg, t: Reg, i: Int): Word = Word(Bits("000101") ++ regToBinary(s) ++ regToBinary(t) ++ encodeSigned(i,  16) )
  def JR(s: Reg): Word = Word(sixZero ++ regToBinary(s) ++ Bits("000000000000000001000"))
  def JALR(s: Reg): Word = Word(sixZero ++ regToBinary(s) ++ Bits("000000000000000001001"))
}
