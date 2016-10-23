package jmms

import jmms.JToken._
import jmms.SyntaxTree.JExpr._

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}



/**
  * Created by weijiayi on 20/10/2016.
  */
object SyntaxParser extends Parsers {
//  override def log[T](p: => Parser[T])(name: String) = Parser{ in => p(in)} // control the log here
  def logName[T](name: String)(p: => Parser[T]) = log(p)(name)// control the log here

  import SyntaxTree._

  def parseSource[T](parser: Parser[T], code: String, sourceName: String = "not specified") ={
    Tokenizer.tokenizeSource(code) match{
      case Right(tokens) =>
        val input = JTokenReader(tokens)
        parseAll(parser, input) match{
          case Success(r, _) => r
          case other =>
            println(s"source file: $sourceName")
            throw new Exception(other.toString)
        }
      case Left(msg) =>
        throw new Exception(msg._1)
    }
  }

  class JTokenReader(val tokens: Array[JToken], tokenOffset: Int) extends Input {
    override def offset: Int = tokenOffset

    override def first: JToken = if(offset < tokens.length) tokens(offset) else EndOfTokens

    override def rest: Reader[JToken] = if(offset < tokens.length) new JTokenReader(tokens, offset+1) else this

    override def pos: Position = first.pos

    override def atEnd: Boolean = offset >= tokens.length

    override def toString: String = first.pos.toString() + "\n" + first.pos.longString
  }

  object JTokenReader{
    def apply(tokens: Iterable[JToken]) = new JTokenReader(tokens.toArray, 0)
  }


  import jmms.{JKeyword => kw}

  override type Elem = JToken

  def pIdent = acceptMatch("identifier", {
    case a@ TIdentifier(name) => a
  })

  implicit def keywordToParser(k: kw.Value): Parser[TReserve] = acceptMatch(s"keyword [${kw.keywordArray(k.id)}]", {
    case a@ TReserve(key) if key == k => a
  })

  def pSep(s: String) = acceptMatch(s"separator [$s]", {
    case a@ TSep(n) if n==s => a
  })

  def pOp(s: String) = acceptMatch(s"operator [$s]", {
    case a@ TOp(n) if n==s => a
  })

  def pOp(ops: String*) = acceptMatch(s"operator [${ops.mkString(" or ")}]", {
    case a@ TOp(n) if ops.contains(n) => a
  })

  def parseAll[A](p: Parser[A], input: Input) = (p <~ end)(input)

  def end = Parser{ in =>
    val reader = in.asInstanceOf[JTokenReader]
    if(reader.atEnd) Success(EndOfTokens, in) else Failure("End of tokens expected", in) }


  def pCompilationUnit: Parser[CompilationUnit] =
    ((kw.k_package ~!> pQualified <~ pSep(";"))
      ~! rep(kw.k_import ~!> pQualified <~ pSep(";"))
      ~! rep(pClassDecl)) ^^ { case p ~ imp ~ cls => CompilationUnit(p,imp.toIndexedSeq, cls.toIndexedSeq) }

  def pQualified = rep1sep(pIdent, TSep(".")) ^^
    { ns => QualifiedIdent(ns.toIndexedSeq)}withFailureMessage "qualified identifier expected"

  def pModifiers: Parser[Modifiers] =
    rep(kw.k_public | kw.k_protected | kw.k_private | kw.k_static | kw.k_abstract) ^^ {ms => Modifiers(ms.toIndexedSeq)}

  def pClassDecl: Parser[ClassDecl] =
    pModifiers ~ (kw.k_class ~!> pIdent) ~! opt(kw.k_extends ~!> pQualified) ~! pClassBody ^^ {
      case ms ~ name ~ inheritance ~ body => ClassDecl(ms, name, inheritance, body)
    }

  def pClassBody: Parser[IndexedSeq[(Modifiers, MemberDecl)]] =
    pSep("{") ~!>
      (rep((pModifiers ~ pMember) ^^ {case a~b => (a,b)}) ^^ {_.toIndexedSeq} ) <~ pSep("}")

  def pMember: Parser[MemberDecl] = pConstructor | pField | pMethod

  def pMethod: Parser[MethodMemberDecl] = logName("method") {
    pType ~ pIdent ~ pFormalParameters ~ (pSep(";") ^^ { _ => None } | (pBlock ^^ { x => Some(x) })) ^^ {
      case t ~ name ~ params ~ body => MethodMemberDecl(t, name, params, body)
    }
  }

  def pField = pVarDecl ^^ FieldMemberDecl

  def pConstructor: Parser[ConstructorDecl] = logName("Constructor") {
    pIdent ~ pFormalParameters ~ pBlock ^^ { case a ~ b ~ c => ConstructorDecl(a, b, c) }
  }

  def pFormalParameters: Parser[IndexedSeq[FormalParameter]] = logName("formal params") {
    pSep("(") ~!> repsep(
      pType ~! pIdent ^^ { case t ~ i => FormalParameter(t, i) }, pSep(",")
    ) <~ pSep(")") ^^ { ps => ps.toIndexedSeq }
  }

  def pType = pRefType | pBasicType

  def pBasicType = (kw.k_boolean | kw.k_int | kw.k_char | kw.k_void) ^^ JBasicType withFailureMessage "Basic type expected"

  def pSquare = pSep("[")~pSep("]")

  def pRefType =
    ( pBasicType ~ rep1(pSquare) ^^ {case t~squares => BasicTypeArray(t, squares.length)} ) |
      (pQualified ~ rep(pSquare) ^^ {case t~squares => RefTypeOrArray(t, squares.length)})

  def pLiteral = acceptMatch("j-- literal", {
    case a@ TInt(int) => a
    case a@ TString(s) => a
    case a@ TChar(c) => a
    case a@ TReserve(kw.k_true) => a
    case a@ TReserve(kw.k_false) => a
    case a@ TReserve(kw.k_null) => a
  }) ^^ JLiteral

  def pArguments = pSep("(") ~> repsep(pExpr, pSep(",")) <~ pSep(")") ^^ {args => JArguments(args.toIndexedSeq)}

  def pBlock: Parser[Block] =
    pSep("{") ~> rep(pBlockStatement) <~ pSep("}") ^^ {x => Block(x.toIndexedSeq)}

  def pBlockStatement = (pVarDecl ^^ LocalVarDecl) | pStatement

  def pVarDecl =
    pType ~ pIdent ~ opt(pOp("=") ~!> pVarInit) <~ pSep(";") ^^ { case t~name~init => VarDecl(t,name,init)}

  def pVarInit: Parser[VarInit] = logName("var init") {
    (pExpr ^^ ExprInit) |
      (pSep("{") ~!> repsep(pVarInit, pSep(",")) <~ pSep("}") ^^ {x => ArrayInit(x.toIndexedSeq)})
  }


  def pIf = logName("if") {
    kw.k_if ~!> pParenExpr ~ pStatement ~ opt(kw.k_else ~!> pStatement) ^^ {
      case cond~b1~b2 => IfStatement(cond,b1,b2)
    }
  }

  def pWhile = kw.k_while ~!> pParenExpr ~ pStatement ^^ {case cond~b => WhileStatement(cond, b)}

  def pReturn = kw.k_return ~!> opt(pExpr) <~ pSep(";") ^^ ReturnStatement

  def pStatement: Parser[Statement] = logName("statement"){
    pBlock | pIf | pWhile | pReturn | (pExpr <~ pSep(";"))
  } withFailureMessage "A statement is expected"

  // expressions

  def pExpr: Parser[JExpr] =  logName("expr"){pAssignment} withFailureMessage "a valid expression expected"

  def parseBinary(nextLayer: Parser[JExpr], ops: String*) = nextLayer ~ rep(pOp(ops :_*) ~! nextLayer) ^^ {
    case p1 ~ List() => p1
    case p1 ~ ((op2 ~ p2) ::ps) =>
      ps.foldLeft(BinaryExpr(op2, p1,p2)){ case (acc, (op~r)) => BinaryExpr(op, acc, r)}
  }

  def pAssignment= parseBinary(pConditionOr, "=", "+=", "-=", "*=", "/=")

  def pConditionOr = parseBinary(pConditionAnd, "||")

  def pConditionAnd = parseBinary(pEquality, "&&")

  def pEquality = parseBinary(pRelation, "==")

  def pRelation = parseBinary(pPlus, ">", "<", ">=", "<=") // todo: instance of

  def pPlus = parseBinary(pMultiply, "+", "-")

  def pMultiply = parseBinary(pUnary, "*", "/")

  def pUnary: Parser[JExpr] =
    log(pOp("-") ~!> pSimpleUnary ^^ UnaryNegate)("unary negation") | pSimpleUnary

  def pSimpleUnary: Parser[JExpr] = logName("simple unary") {
    (pOp("!") ~!> pUnary ^^ UnaryNot) |
      ((pSep("(") ~> pBasicType <~ pSep(")")) ~ pUnary ^^ { case t ~ u => BasicCast(t, u) }) |
      ((pSep("(") ~> pRefType <~ pSep(")")) ~ pSimpleUnary ^^ { case t ~ u => RefTypeCast(t, u) }) |
      pPostfix
  }

  def pPostfix = logName("postfix") {
    pPrimary ~ rep(pSelector) ^^ { case p ~ selectos =>
      if (selectos.isEmpty) p
      else PostExpr(p, selectos.toIndexedSeq)
    }
  }

  def pSelector = logName("selector") {
    (pSep(".") ~!> pQualified ~ opt(pArguments) ^^ { case b ~ a => JQualifiedSelector(b, a) }) |
      (pSep("[") ~!> pExpr <~ pSep("]") ^^ JArraySelector)
  }

  def pParenExpr = pSep("(") ~!> pExpr <~ pSep(")")

  def pPrimary: Parser[JExpr] = logName("primary") {
      pParenExpr |
      pLiteral |
      (pQualified ~ opt(pArguments) ^^ { case q ~ args =>
        if(args.isEmpty) q
        else MethodCall(q, args.get)
      })|
      pCreator|
      (kw.k_this ~!> opt(pArguments) ^^ { args =>
        if(args.isEmpty) JThis
        else ConstructorCall(args.get)
      })|
      (kw.k_super ~!>
        (pArguments ^^ {ConstructorCall(_, isSuper = true)} |
          (pSep(".") ~!> pIdent ~ opt(pArguments)) ^^ { case name~args =>
            if(args.isEmpty) SuperIdent(name)
            else MethodCall(QualifiedIdent(name), args.get, isSuper = true)
          }))
  }

  def pCreator: Parser[JCreator] = kw.k_new ~!> pRefType ~! pArguments ^^ {
    case t~args => JCreator(t,args)
  }

  // end of parsers

  /** More handy parser operators */
  implicit class ExtendParserOps[T,A](p0: A)(implicit convert: A => Parser[T]) {
    val p = convert(p0)

    def ~!>[U](q: Parser[U]) = p ~! q ^^ {case _ ~ r => r}
//    def <!~[U](q: Parser[U]) = p ~! q ^^ {case r ~ _ => r}
  }
}
