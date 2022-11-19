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
import cs241e.scanparse.Grammars._

import scala.Seq
import scala.collection.mutable

/** Implementation of context-sensitive analysis for the Lacs language. */

object Typer {
  /** Representation of a Lacs type, which is either an Int or a function type with parameter types and a return type.
    */
  sealed abstract class Type
  case object IntType extends Type
  case class FunctionType(parameterTypes: Seq[Type], returnType: Type) extends Type

  /** Given a `tree`, finds all descendants of the `tree` whose root node has kind `lhsKind`.
    * Does not search within the found subtrees for any nested occurrences of additional descendants.
    *
    * For example, searching the root of a program tree with `lhsKind = "procedure"` will return the trees all
    * of the top-level procedures, but not any procedures nested within them.
    */
  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))

  /** Given a tree that is either a "type" or contains exactly one "type" nested within it, returns
    * an instance of `Type` representing the corresponding type.
    */
  def parseType(tree: Tree): Type = {
    val types = collect(tree, "type")
    require(types.size == 1)
    val root = types.head;
    if(root.children.length == 1){ // M
      return IntType;
    }
    //Production rule is: type LPAREN typesopt RPAREN ARROW type
    val returnType = parseType(root.children(4))

    // Type is function type
    val argTypesTree = root.children(1)
    if(argTypesTree.children.isEmpty){
      return FunctionType(Seq(), returnType)
    }

    def unwrapTypes(typesTree: Tree): Seq[Type] = {
      // Production rule: types type COMMA types
      if(typesTree.children.isEmpty){
        return Seq();
      }

      val rest = if (typesTree.children.length >= 2) unwrapTypes(typesTree.children(2)) else Seq()
      return Seq(parseType(typesTree.children.head)) ++ rest
    }

    return FunctionType(unwrapTypes(argTypesTree.children.head), returnType)
  }

  /** A variable combined with its declared type. */
  case class TypedVariable(variable: Variable, tpe: Type)

  /** Create a new `Variable` given its `name` and type `tpe`. */
  def makeVariable(name: String, tpe: Type): Variable =
    new Variable(name, isPointer = (tpe != IntType))

  /** A `SymbolTable` maps each name to either a `TypedVariable` or a `ProcedureScope`. */
  type SymbolTable = Map[String, Either[TypedVariable, ProcedureScope]]

  /** Given a tree containing subtrees rooted at "vardef", creates a `TypedVariable` for each such tree. */
  def parseVarDefs(tree: Tree): Seq[TypedVariable] = {
    collect(tree, "vardef").map{ varDef => TypedVariable(
      makeVariable(varDef.children.head.lhs.lexeme,  // Get ID
        parseType(varDef)), parseType(varDef))}
  }

  /** Call `sys.error()` if any `String` occurs in `names` multiple times. */
  def checkDuplicates(names: Seq[String]): Unit = {
    val duplicates = names.diff(names.distinct)
    if(duplicates.nonEmpty) sys.error(s"Duplicate identifiers ${duplicates}")
  }

  /** A `ProcedureScope` holds the semantic information about a particular procedure that is needed to type-check
    * the body of the procedure, including information coming from outer procedure(s) within which this
    * procedure may be nested.
    *
    * @param tree the tree defining the procedure (rooted at a "defdef")
    * @param outer the `ProcedureScope` of the outer procedure that immediately contains this one
    */
  class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
    assert(tree.production ==
      "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE")
    val Seq(_, id, _, parmsopt, _, _, retTypeTree, _, _, vardefs, defdefs, expras, _) = tree.children

    /** The name of the procedure. */
    val name: String = ???

    /** The parameters of the procedure. */
    val parms: Seq[TypedVariable] = ???

    /** The variables declared in the procedure. */
    val variables: Seq[TypedVariable] = ???

    /** The declared return type of the procedure. */
    val returnType: Type = ???

    /** The type of the procedure. */
    val tpe: FunctionType = ???

    /** The new `Procedure` object that will represent this procedure. */
    val procedure: Procedure = ???

    /** The `ProcedureScope`s of the nested procedures that are immediately nested within this procedure.
      *
      * Note: this `val` will recursively call `new ProcedureScope(...)`.
      */
    val subProcedures: Seq[ProcedureScope] = ???

    /** The names of parameters, variables, and nested procedures that are newly defined within this procedure
      * (as opposed to being inherited from some outer procedure).
      */
    val newNames: Seq[String] = ???
    checkDuplicates(newNames)

    /** Create and return a symbol table to be used when type-checking the body of this procedure. It
      * should contain all symbols (parameters, variables, nested procedures) defined in this procedure,
      * as well as those defined in outer procedures within which this one is nested. Symbols defined in
      * this procedure override (shadow) those of outer procedures. The `outerSymbolTable` parameter
      * contains the symbol table of the enclosing scope (either an outer procedure within which the
      * current procedure is nested, or, if the current procedure is a top-level procedure, a symbol
      * table containing the names of all of the top-level procedures).
      */
    def symbolTable(outerSymbolTable: SymbolTable): SymbolTable = ???

    /** Returns a sequence containing `this` `ProcedureScope` and the `ProcedureScope`s for all procedures
      * declared inside of this procedure, including those nested recursively within other nested procedures.
      *
      * Scala hint: learn about the `flatMap` method in the Scala library. If you are not familiar with flatMap,
      * one place you can read about it is here:
      * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
      */
    def descendantScopes: Seq[ProcedureScope] = ???

    override def toString = s"ProcedureScope for $name"
  }

  /** Creates a map containing a symbol table for each procedure scope by calling the scope's symbolTable method,
    * passing in the symbol table of its outer enclosing procedure (or the top level symbol table for a top level
    * procedure).
    */
  def createSymbolTables(topLevelProcedureScopes: Seq[ProcedureScope], topLevelSymbolTable: SymbolTable):
    Map[ProcedureScope, SymbolTable] = {
    def recur(procedureScopes: Seq[ProcedureScope], outerSymbolTable: SymbolTable): Map[ProcedureScope, SymbolTable] = {
      procedureScopes.flatMap{ procedureScope =>
        val symbolTable = procedureScope.symbolTable(outerSymbolTable)
        Map(procedureScope -> symbolTable) ++ recur(procedureScope.subProcedures, symbolTable)
      }.toMap
    }
    recur(topLevelProcedureScopes, topLevelSymbolTable)
  }

  /** Checks that the body of a procedure satisfies the type-checking rules in the Lacs language specification.
    * Returns a `Map` that provides a `Type` for each `Tree` that has a `Type` according to the language
    * specification.
    */

  def typeCheck(scope: ProcedureScope, symbolTable: SymbolTable): Map[Tree, Type] = {
    /** The map that will be returned containing the `Type` of each `Tree` that has a `Type`. */
    val treeToType = mutable.Map[Tree, Type]()

    /** Calls `sys.error()` if `tpe1` and `tpe2` are not equal. If they are equal, returns them. */
    def mustEqual(tpe1: Type, tpe2: Type): Type =
      if(tpe1 == tpe2) tpe1 else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")

    /** For a `tree` rooted at a node that has a `Type`, computes the `Type`, adds it to `treeToType`,
      * and returns it.
      */
    def typeOf(tree: Tree): Type = {
      def fun: Type = {
        ???
      }
      treeToType.getOrElseUpdate(tree, fun)
    }

    /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
    mustEqual(scope.returnType, typeOf(scope.expras))
    
    Map() ++ treeToType
  }

  /** A data structure representing the result of context-sensitive analysis of a whole Lacs program.
    *
    * @param procedureScopes the `ProcedureScopes` representing the semantic information about each procedure.
    * @param symbolTables a symbol table for each procedure in the program.
    * @param typeMap result of type-checking: provides a type for each tree node that represents an expression.
    */
  case class TypedProcedures(
                              procedureScopes: Seq[ProcedureScope],
                              symbolTables: ProcedureScope=>SymbolTable,
                              typeMap: PartialFunction[Tree,Type]) {

    /** Output human-readable form of a parse tree (for debugging purposes) annotated with the type information
      * for tree nodes that have a type.
      **/
    def showTree(tree: Tree, indent: Int = 0): String = {
      val typeString = typeMap.lift.apply(tree) match {
        case Some(tpe) => ": " + tpe
        case None => ""
      }
      " " * indent + tree.lhs + typeString + "\n" +
        tree.children.map(ch => showTree(ch, indent+1)).mkString
    }

    override def toString = procedureScopes.map{ procedureScope =>
      procedureScope.toString + "\n" +
        "Symbol table:\n" +
        symbolTables(procedureScope).map{case (name, meaning) => s" $name -> $meaning\n"}.mkString +
        "Procedure body with types:\n" +
        showTree(procedureScope.expras)
    }.mkString("\n")
  }

  /** Type-checks a Lacs program parse tree. Returns `TypedProcedures`, which contains the `ProcedureScope`s
    * representing the procedures, a map giving a `SymbolTable` for each `ProcedureScope`,
    * and a map giving the `Type` of each `Tree` that has one.
    */
  def typeTree(tree: Tree): TypedProcedures = {
    assert(tree.production == "S BOF defdefs EOF")
    val defdefs = tree.children(1)

    val topLevelProcedureScopes = collect(defdefs, "defdef").map{defdef => new ProcedureScope(defdef, None)}
    checkDuplicates(topLevelProcedureScopes.map(procedureScope => procedureScope.name))
    val topLevelSymbolTable: SymbolTable =
      topLevelProcedureScopes.map{procedure => (procedure.name -> Right(procedure))}.toMap
    val symbolTables = createSymbolTables(topLevelProcedureScopes, topLevelSymbolTable)

    val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope => procedureScope.descendantScopes)

    val typeMap: Map[Tree, Type] = allProcedureScopes.flatMap(procedureScope =>
      typeCheck(procedureScope, symbolTables(procedureScope))).toMap

    val mainProcedure = topLevelProcedureScopes.head
    if(mainProcedure.tpe != FunctionType(Seq(IntType, IntType), IntType))
      sys.error("The type of the main procedure must be (Int, Int)=>Int.")

    TypedProcedures(allProcedureScopes, symbolTables, typeMap)
  }
}
