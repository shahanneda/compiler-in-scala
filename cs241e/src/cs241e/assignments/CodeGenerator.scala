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

import cs241e._
import ProgramRepresentation._
import CodeBuilders._
import Typer._
import cs241e.assignments.Assembler.{LIS, encodeUnsigned}
import cs241e.mips.Word
import scanparse.Grammars._

/** A code generator that converts a Lacs parse tree into the intermediate language developed in Assignment 1 to 6. */
object CodeGenerator {
  def generateProcedures(typedProcedures: TypedProcedures) = {

    /** Given a `procedureScope`, generates the `Code` implementing the procedure, and assigns it to
      * `procedureScope.procedure.code`.
      */
    def generateCode(procedureScope: ProcedureScope): Unit = {

      val symbolTable = typedProcedures.symbolTables(procedureScope)
     def unWrap(expras: Tree): Seq[Tree] = {
        //rule: expras expra SEMI expras
        if (expras.children.length == 1) {
          return Seq(expras.children.head);
        }
        return Seq(expras.children.head) ++ unWrap(expras.children(2))
      }
      /** Generates the `Code` that implements `tree`.
        *
        * This method will be called from the outside only on `tree`s rooted at nodes of kind "expras".
        * However, if it calls itself recursively on other kinds of nodes, it needs to be able to handle
        * those node kinds as well.
        */

      def recur(tree: Tree): Code = {
        tree.production match {
          case "expras expra SEMI expras" =>{
            val Seq(e1, _, e2) = tree.children;
            //println("in semi case" + e1 + e2)
            block(recur(e1), recur(e2))
          }
          case "expras expra" =>{
            val Seq(e1) = tree.children;
            //println("in here" + e1)
            val out = recur(e1)
            //println(out)
            out
          }
          case "expra ID BECOMES expr" =>{
            val Seq(id, _, e2) = tree.children;
            val idname = id.lhs.lexeme;
            symbolTable(idname) match {
              case Left(value: TypedVariable) => block(recur(e2), write(value.variable, Reg.result))
              case Right(value: ProcedureScope) => sys.error("Invalid assigment, trying to assign to procedure!")
            }
          }
          case "expra expr" => {
            val Seq(e) = tree.children;
            recur(e)
          }
          case "expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE" => {
            val Seq(_, _, test, _, _, expras, _, _, _, elseExpras, _) = tree.children;
            val Seq(a,cmp, b) = test.children;
            var finalCmp: (Label) => Code = cmp.lhs.kind match {
              case "NE" => neCmp
              case "LT" => ltCmp
              case "LE" => leCmp
              case "GE" => geCmp
              case "GT" => gtCmp
              case "EQ" => eqCmp
            }
            ifStmt(recur(a), finalCmp, recur(b), recur(expras), recur(elseExpras))
          }
          case "expr term" => {
            val Seq(e) = tree.children;
            recur(e)
          }
          case "expr expr PLUS term" => {
            val Seq(e1, _, e2) = tree.children;
            binOp(recur(e1), plus, recur(e2))
          }
          case "expr expr MINUS term" => {
            val Seq(e1, _, e2) = tree.children;
            binOp(recur(e1), minus, recur(e2))
          }
          case "term factor" => {
            val Seq(e) = tree.children;
            recur(e)
          }
          case "term term STAR factor" => {
            val Seq(e1, _, e2) = tree.children;
            binOp(recur(e1), times, recur(e2))
          }
          case "term term SLASH factor" => {
            val Seq(e1, _, e2) = tree.children;
            binOp(recur(e1), divide, recur(e2))
          }
          case "term term PCT factor" => {
            val Seq(e1, _, e2) = tree.children;
            binOp(recur(e1), remainder, recur(e2))
          }
          case "factor ID" | "factor ID " =>{
            val Seq(e) = tree.children;
            val sym = symbolTable(e.lhs.lexeme)
            sym match {
              case Left(value: TypedVariable) => return read(Reg.result, value.variable)
              case Right(value: ProcedureScope) => return Closure(value.procedure)
            }
          }
          case "factor NUM" =>{
            val Seq(e) = tree.children;
            block(
              LIS(Reg.result),
              Word(encodeUnsigned(Integer.valueOf(e.lhs.lexeme).toLong))
            )
          }

          case "factor LPAREN expr RPAREN" => {
            val Seq(_, e, _) = tree.children;
            recur(e)
          }

          case "factor factor LPAREN argsopt RPAREN" => {
            val Seq(factor, _, argsopt, _) = tree.children;
            // TODO: should I not be using the symbol table for this?
            val tmpVar = new Variable("tmp closure var");

            if(factor.production == "factor ID"){
              val Seq(id) = factor.children
              val symbol = symbolTable(id.lhs.lexeme) match {
                case Left(value: TypedVariable) => {
                  if(argsopt.children.isEmpty)
                    return CallClosure(recur(factor), Seq(), Seq())
                  else {
                    return Scope(Seq(tmpVar), block(
                      recur(factor),
                      write(tmpVar, Reg.result),
                      CallClosure(readVarRes(tmpVar), unWrap(argsopt.children.head).map(x => recur(x)), unWrap(argsopt.children.head).map(x => new Variable("")))
                    ))
                  }
                }
                case Right(value: ProcedureScope) => {
                  if(argsopt.children.isEmpty) {
                    return Call(value.procedure, Seq())
                  } else {
                    return Call(value.procedure, unWrap(argsopt.children.head).map(x => recur(x)))
                  }
                }
              }
            }
             block(
              if(argsopt.children.isEmpty)
                CallClosure(recur(factor), Seq(), Seq())
              else {
                val factorTree = factor
                Scope(Seq(tmpVar), block(
                  recur(factor),
                  write(tmpVar, Reg.result),
                  CallClosure(readVarRes(tmpVar), unWrap(argsopt.children.head).map(x => recur(x)), unWrap(argsopt.children.head).map(x => new Variable("")))
                ))
              }
            )
          }
          case x => sys.error("did not match" + tree + " peodution is -" + tree.production + "-")
        }
      }

      /* Main body of generateCode. */
      procedureScope.procedure.code = Scope(procedureScope.variables.map(_.variable), recur(procedureScope.expras))
    }

    /* Main body of generateProcedures. */

    typedProcedures.procedureScopes.foreach(generateCode)
    typedProcedures.procedureScopes.map(_.procedure)
  }
}
