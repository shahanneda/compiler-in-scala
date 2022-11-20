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
    if(root.children.length == 1){ // Must be int type
      return IntType;
    }

    // Must be functoin type
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
    val name: String = id.lhs.lexeme

    /** The parameters of the procedure. */
    val parms: Seq[TypedVariable] = collect(parmsopt, "parms").flatMap((parm) => {
      def unwrapTypes(parm: Tree): Seq[TypedVariable] = {
        if(parm.children.isEmpty){
          return Seq()
        } else if (parm.children.length == 1){
          //parms vardef
          val Seq(id, _, typ) = parm.children.head.children

          return Seq(TypedVariable(makeVariable(id.lhs.lexeme, parseType(typ)), parseType(typ)))
        }else if (parm.children.length == 3){
          //parms vardef COMMA parms

          val Seq(vardef, _, parms) = parm.children
          val Seq(id, _, typ) = vardef.children

          return Seq(TypedVariable(makeVariable(id.lhs.lexeme, parseType(typ)), parseType(typ))) ++ unwrapTypes(parms)
        }
        sys.error("Unexpected parms!" + parms);
        Seq()
      }
      unwrapTypes(parm)
    })

    /** The variables declared in the procedure. */
    val variables: Seq[TypedVariable] = parseVarDefs(vardefs)

    /** The declared return type of the procedure. */
    val returnType: Type = parseType(retTypeTree)

    /** The type of the procedure. */
    val tpe: FunctionType = new FunctionType(parms.map(x => x.tpe), returnType)

    /** The new `Procedure` object that will represent this procedure. */
    val procedure: Procedure = new Procedure(name, parms.map(x => x.variable), outer=(if(outer.isDefined) Option(outer.get.procedure) else None))

    /** The `ProcedureScope`s of the nested procedures that are immediately nested within this procedure.
      *
      * Note: this `val` will recursively call `new ProcedureScope(...)`.
      */
    val subProcedures: Seq[ProcedureScope] = {
      def helper(currentDefDef: Tree): Seq[Tree] ={
        if(currentDefDef.children.length == 1){
          val Seq(defdef) = currentDefDef.children;
          return Seq(defdef);
        }else{
          val Seq(defdef, newDefDefs) = currentDefDef.children;
          return Seq(defdef) ++ helper(newDefDefs)
        }
      }
      if(defdefs.production == "defdefsopt defdefs"){
        helper(defdefs.children.head).map(x => {
          val out = new ProcedureScope(x, Option(this))
          //println(" created " +  out+ "with "+ x+ " with outer "+ out.procedure.outer)
          out
        })
      }else{
        Seq()
      }
    }

    /** The names of parameters, variables, and nested procedures that are newly defined within this procedure
      * (as opposed to being inherited from some outer procedure).
      */
    val newNames: Seq[String] = parms.map(x => x.variable.name) ++ variables.map(x => x.variable.name) ++ subProcedures.map(x => x.name)
    checkDuplicates(newNames)

    /** Create and return a symbol table to be used when type-checking the body of this procedure. It
      * should contain all symbols (parameters, variables, nested procedures) defined in this procedure,
      * as well as those defined in outer procedures within which this one is nested. Symbols defined in
      * this procedure override (shadow) those of outer procedures. The `outerSymbolTable` parameter
      * contains the symbol table of the enclosing scope (either an outer procedure within which the
      * current procedure is nested, or, if the current procedure is a top-level procedure, a symbol
      * table containing the names of all of the top-level procedures).
      */
      // TODO: change ordering of tis if needed
    def symbolTable(outerSymbolTable: SymbolTable): SymbolTable = outerSymbolTable ++ parms.map(x => (x.variable.name, Left(x))) ++ variables.map(x => (x.variable.name, Left(x))) ++ subProcedures.map(x => (x.name, Right(x)))

    /** Returns a sequence containing `this` `ProcedureScope` and the `ProcedureScope`s for all procedures
      * declared inside of this procedure, including those nested recursively within other nested procedures.
      *
      * Scala hint: learn about the `flatMap` method in the Scala library. If you are not familiar with flatMap,
      * one place you can read about it is here:
      * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
      */
    def descendantScopes: Seq[ProcedureScope] = Seq(this) ++ subProcedures.flatMap(x => x.descendantScopes)

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
      if (tpe1 == tpe2) tpe1 else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")

    /** For a `tree` rooted at a node that has a `Type`, computes the `Type`, adds it to `treeToType`,
     * and returns it.
     */
    def typeOf(tree: Tree): Type = {
      //println(tree)

      def unWrap(expras: Tree): Seq[Tree] = {
        //rule: expras expra SEMI expras
        if (expras.children.length == 1) {
          return Seq(expras.children.head);
        }
        return Seq(expras.children.head) ++ unWrap(expras.children(2))
      }

      def fun: Type = {
        if (tree.production == "expras expra SEMI expras" || tree.production == "expras expra") {
          return unWrap(tree).map(x => typeOf(x)).last;
        }else if (tree.production == "expra expr") {
          assert(tree.children.head.lhs.kind == "expr");
          return typeOf(tree.children.head)
        } else if (tree.production == "expra ID BECOMES expr") {
          val Seq(id, _, expr) = tree.children;
          val idType = symbolTable(id.lhs.lexeme) match {
            case Left(value) => value.tpe
            case Right(value: ProcedureScope) => sys.error(id.lhs.lexeme + " must be a variable, not a procedure in assigment!")
          };

          val insideType = typeOf(expr);
          mustEqual(insideType, idType);
          return insideType;
        }
        else if (tree.production == "expr term") {
          val term = tree.children.head;
          return typeOf(term);
        }else if(tree.production == "expr expr PLUS term" || tree.production == "expr expr MINUS term"){
          val Seq(e, _, t) = tree.children;
          mustEqual(typeOf(e), IntType);
          mustEqual(typeOf(t), IntType);
          return IntType;
        }else if (tree.production == "expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE"){
          val Seq(_, _, test, _, _, expras, _, _, _, elseExpras, _) = tree.children;
          val Seq(a,_, b) = test.children;
          mustEqual(typeOf(a), IntType);
          mustEqual(typeOf(b), IntType);
          mustEqual(typeOf(expras), typeOf(elseExpras))
          return typeOf(expras)
        }
        else if (tree.production == "term factor") {
          val Seq(factor) = tree.children;
          return typeOf(factor)
        } else if (tree.production == "term term STAR factor" || tree.production == "term term SLASH factor" || tree.production == "term term PCT factor"){
          val Seq(t, _, f) = tree.children;
          mustEqual(typeOf(t), IntType);
          mustEqual(typeOf(f), IntType);
          return IntType;
        }
        else if (tree.production == "factor NUM") {
          return IntType;
        } else if (tree.production == "factor ID") {
          val id = tree.children.head.lhs.lexeme;
          val symbolLookup: Either[TypedVariable, ProcedureScope] = symbolTable(id)
          symbolLookup match {
            case Left(value: TypedVariable) => return value.tpe
            case Right(value: ProcedureScope) => return value.tpe
          }
        }
        else if (tree.production == "factor LPAREN expr RPAREN") {
          val Seq(_, expr, _) = tree.children;
          return typeOf(expr);
        }
        else if (tree.production == "factor factor LPAREN argsopt RPAREN") {
          val Seq(factor2, _, argsopt, _) = tree.children;
          val factorType = typeOf(factor2);
          factorType match {
            case IntType => sys.error("Type Mismatch: Got int, expected a function!")
            case FunctionType(parameterTypes, returnType) => {
              if (argsopt.children.isEmpty) {
                // argsopt
                if (parameterTypes.nonEmpty) {
                  sys.error("Calling function with no args, got args: " + argsopt);
                }
                return returnType;
              }
              //argsopt args
              val argTypes = unWrap(argsopt.children.head).map(x => typeOf(x))
              if (argTypes.length != parameterTypes.length) {
                sys.error("Expected " + parameterTypes.length + " args, got:" + argTypes.length)
              }
              parameterTypes.toList.zipWithIndex.foreach { case (typ, i) => mustEqual( argTypes(i), typ) }
              return returnType;
            }
          }
        }
        //println(tree);
        sys.error("could not type" + tree)
      }

      treeToType.getOrElseUpdate(tree, fun)
    }

    /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
    mustEqual(scope.returnType, typeOf(scope.expras))

    //println("all trees: ");
    //println(treeToType)
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
