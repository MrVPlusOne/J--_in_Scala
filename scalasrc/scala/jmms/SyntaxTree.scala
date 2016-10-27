package jmms

import jmms.JToken._
import jmms.SyntaxTree.JExpr._

/**
  * Created by weijiayi on 21/10/2016.
  */
object SyntaxTree{

  /** help functions for writing Syntax Trees, used for testing purpose.
    * No position is set for JTokens */
  object Pen{
    def qualified(names: String) = QualifiedIdent(names.split("\\.").map(TIdentifier).toIndexedSeq)
    def multiply(l: JExpr, r: JExpr) = BinaryExpr(TOp("*"), l, r)
    def plus(l: JExpr, r: JExpr) = BinaryExpr(TOp("+"), l, r)
    def minus(l: JExpr, r: JExpr) = BinaryExpr(TOp("-"), l, r)

    def int_type = JBasicType(TReserve(JKeyword.k_int))
    def boolean_type = JBasicType(TReserve(JKeyword.k_boolean))
    def char_type = JBasicType(TReserve(JKeyword.k_char))

    def int(i: Int) = JLiteral(TInt(i))
    def string(s: String) = JLiteral(TString(s))

    def k_true = JLiteral(TReserve(JKeyword.k_true))
    def k_false = JLiteral(TReserve(JKeyword.k_false))
  }

  /** a package */
  case class CompilationUnit(
                              packageDeclaration: QualifiedIdent,
                              imports: IndexedSeq[QualifiedIdent],
                              classDecls: IndexedSeq[ClassDecl]
                            ) extends SyntaxTree{

    override def children: IndexedSeq[SyntaxTree] = (packageDeclaration +: imports) ++ classDecls

    override def nodeName: String = "Package"
  }

  case class Modifiers(children: IndexedSeq[TReserve]) extends SyntaxTree{
    override def nodeName: String = "Modifiers"

    def isAbstract = children contains TReserve(JKeyword.k_abstract)

    def isStatic = children contains TReserve(JKeyword.k_static)
  }

  /** declaration of a class */
  case class ClassDecl(modifiers: Modifiers, name: TIdentifier,
                       inheritance: Option[QualifiedIdent], body: IndexedSeq[(Modifiers, MemberDecl)])
      extends SyntaxTree {
    override def children: IndexedSeq[SyntaxTree] =
      IndexedSeq(modifiers, name) ++ inheritance.toIndexedSeq ++ body.flatMap({case (mf,md) => IndexedSeq(mf,md)})

    override def nodeName: String = "Class"
  }

  /** a member of a class */
  sealed trait MemberDecl extends SyntaxTree

  /** a j-- method */
  case class MethodMemberDecl(jType: JType, name: TIdentifier,
                              formalParams: IndexedSeq[FormalParameter], body: Option[Block]) extends MemberDecl{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(jType, name) ++ formalParams ++ body.toIndexedSeq

    override def nodeName: String = "Method"
  }

  /** a j-- field */
  case class FieldMemberDecl(variableDecl: VarDecl) extends MemberDecl{
    override def children: IndexedSeq[SyntaxTree] = single(variableDecl)

    override def nodeName: String = "Field"
  }

  case class FormalParameter(jType: JType, identifier: TIdentifier) extends SyntaxTree{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(jType, identifier)

    override def nodeName: String = "Param"
  }

  case class ConstructorDecl(name: TIdentifier,
                             formalParams: IndexedSeq[FormalParameter], block: Block) extends MemberDecl{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(name) ++ formalParams :+ block

    override def nodeName: String = "Constructor"
  }

  case class VarDecl(formalParameter: FormalParameter, initializer: Option[VarInit]) extends SyntaxTree {
  override def children: IndexedSeq[SyntaxTree] = formalParameter +: initializer.toIndexedSeq

    override def nodeName: String = "Variable Decl"
  }

  sealed trait VarInit extends JExpr

  case class ExprInit(expr: JExpr) extends VarInit{
    override def children: IndexedSeq[SyntaxTree] = single(expr)

    override def nodeName: String = "Expr Init"
  }

  case class ArrayInit(children: IndexedSeq[VarInit]) extends VarInit{
    override def nodeName: String = "Array Init"
  }

  object ArrayInit{
    def apply(inits: VarInit*) = new ArrayInit(inits.toIndexedSeq)
  }

  /** a block statement, but can't be variable declaration
    * (because variable declarations must sit in code blocks) */
  sealed trait Statement extends SyntaxTree

  case class LocalVarDecl(variableDecl: VarDecl) extends Statement{
    override def children: IndexedSeq[SyntaxTree] = single(variableDecl)

    override def nodeName: String = "Local Decl"
  }


  case class Block(children: IndexedSeq[Statement]) extends Statement {
    override def nodeName: String = "Block"
  }

  case class IfStatement(condition: JExpr, thenBranch: Block, elseBranch: Option[Block]) extends Statement{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(condition, thenBranch) ++ elseBranch.toIndexedSeq

    override def nodeName: String = "If Statement"
  }

  case class WhileStatement(condition: JExpr, body: Block) extends Statement{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(condition, body)

    override def nodeName: String = "While Statement"
  }

  case class ReturnStatement(expr: Option[JExpr]) extends Statement{
    override def children: IndexedSeq[SyntaxTree] = expr.toIndexedSeq

    override def nodeName: String = "Return"
  }


  case object JNull

  case class JArguments(args: IndexedSeq[JExpr]) extends SyntaxTree{
    override def children: IndexedSeq[SyntaxTree] = args

    override def nodeName: String = "Arguments"
  }

  object JArguments{
    def apply(args: JExpr*) = new JArguments(args.toIndexedSeq)
  }

  case class SuperIdent(identifier: TIdentifier) extends JExpr{
    override def children: IndexedSeq[SyntaxTree] = single(identifier)

    override def nodeName: String = "super."
  }

  case class QualifiedIdent(parts: IndexedSeq[TIdentifier]) extends JExpr {
    override def children: IndexedSeq[SyntaxTree] = parts

    override def nodeName: String = "Qualified"

    def toPath(sep: String) = parts.map(_.data).mkString(sep)

    def toDotPath = toPath(".")
  }

  object QualifiedIdent{
    def apply(parts: TIdentifier*) = new QualifiedIdent(parts.toIndexedSeq)

    def dotPath(parts: TIdentifier*) = parts.map(_.data).mkString(".")
  }

  sealed trait JType extends SyntaxTree{
    override def nodeName: String = "Type"
  }

  case class JBasicType(t: TReserve) extends JType{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(t)

    override def toString: String = s"basic type '${JKeyword.keywordArray(t.data.id)}'"
  }

  case class BasicTypeArray(t: JBasicType, arrayDimensions: Int) extends JType{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(t)

    override def toString: String = s"array type '$t${"[]"*arrayDimensions}'"
  }


  /** a reference type, possibly an array */
  case class RefTypeOrArray(ty: QualifiedIdent, arrayDimensions: Int) extends JType{
    override def children: IndexedSeq[SyntaxTree] = IndexedSeq(ty)

    override def toString: String = s"ref type '${ty.toDotPath}${"[]"*arrayDimensions}'"
  }


//  case class LocalDecl(t: JType, pairs: IndexedSeq[(TIdentifier, JExpr)]) extends BlockStatement{
//    override def children: IndexedSeq[SyntaxTree] = t +: pairs.flatMap{case (a,b) => IndexedSeq(a,b)}
//
//    override def nodeName: String = "Var Decl"
//  }


  // An expression can also be used as a statement (i.e. using its side-effects)
  sealed trait JExpr extends Statement


  object JExpr {

    case class JLiteral(literal: JToken) extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(literal)

      override def nodeName: String = "Literal"
    }

    case object JThis extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq()

      override def nodeName: String = "this"
    }

    case class JCreator(refTypeOrArray: JType, args: JArguments) extends JExpr {
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(refTypeOrArray, args)

      override def nodeName: String = "Creator"
    }

    case class BinaryExpr(op: TOp, left: JExpr, right: JExpr) extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(left, op, right)

      override def nodeName: String = op.data
    }


    sealed trait UnaryExpr extends JExpr{
      def expr: JExpr
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(expr)
    } //todo: ++

    case class UnaryNegate(expr: JExpr) extends UnaryExpr{


      override def nodeName: String = "-"
    }

    case class UnaryNot(expr: JExpr) extends UnaryExpr{
      override def nodeName: String = "Unary!"
    }

    case class BasicCast(basicType: JBasicType, expr: JExpr) extends UnaryExpr{

      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(basicType, expr)

      override def nodeName: String = "Basic Cast"
    }

    case class RefTypeCast(refType: JType, expr: JExpr) extends UnaryExpr{

      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(refType, expr)

      override def nodeName: String = "Ref Cast"
    }

    case class MethodCall(f: QualifiedIdent, args: JArguments, isSuper: Boolean = false) extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(f,args)

      override def nodeName: String = "Method Call"
    }

    case class ConstructorCall(args: JArguments, isSuper: Boolean = false) extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(args)

      override def nodeName: String = "Constructor Call"
    }

    case class PostExpr(body: JExpr, selectors: IndexedSeq[JSelector]) extends JExpr{
      override def children: IndexedSeq[SyntaxTree] = body +: selectors

      override def nodeName: String = "Post Expr"
    }

    sealed trait JSelector extends JExpr

    case class JQualifiedSelector(ident: QualifiedIdent, args: Option[JArguments]) extends JSelector {
      override def children: IndexedSeq[SyntaxTree] = ident +: args.toIndexedSeq

      override def nodeName: String = "Selector"
    }

    case class JArraySelector(arg: JExpr) extends JSelector{
      override def children: IndexedSeq[SyntaxTree] = IndexedSeq(arg)

      override def nodeName: String = "Array Selector"
    }

  }

  def pathToDotInSrc(tree: SyntaxTree, dot: Int): List[SyntaxTree] = {
    def rec(node: SyntaxTree, path: List[SyntaxTree]): List[SyntaxTree] = {
      val newPath = node::path
      if(node.children.isEmpty) newPath
      else node.children.find(_.rangeContains(dot)) match{
        case Some(t1) => rec(t1, newPath)
        case None => newPath
      }
    }

    rec(tree, List())
  }
}

trait SyntaxTree extends Ranged{
  def rangeContains(dot: Int): Boolean = {
    val (l,r) = getRange
    l<=dot && dot < r
  }

  def children: IndexedSeq[SyntaxTree]

  def nodeName: String

  protected def single(node: SyntaxTree) = IndexedSeq(node)

  def foreach(f: SyntaxTree => Unit): Unit =
    if(children.nonEmpty){
      f(this)
      children.foreach(_.foreach(f))
    }
}