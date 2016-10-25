package jmms

import jmms.JToken.TIdentifier
import jmms.SemanticsAnalysis.SemanticError
import jmms.SyntaxTree.JExpr.{BinaryExpr, JLiteral, MethodCall}
import jmms.SyntaxTree._
import jmms.SBasicType._

import scala.collection.mutable.ListBuffer


sealed trait STyped extends Ranged{
  def sType: SType
}

object STyped{

  def requireType(sExpr: STyped, sType: SType): Unit = {
    if(sExpr.sType != sType)
      throw SemanticError(s"Type mismatch, required type: $sType, found ${sExpr.sType}", sExpr)
  }

  object SBlock{
    def apply(block: Block, blockCtx: BlockContext): SBlock = {
      var ctx = blockCtx
      var statements = ListBuffer[STyped]()
      block.children.foreach{
        case LocalVarDecl(VarDecl(formalParameter, initializer)) =>
          val sType = ctx.resolveType(formalParameter.jType)
          ctx += (formalParameter.identifier.data, sType)
        case s: Statement =>
          val sTyped: STyped =  s match {
            case b1: Block =>
              apply(b1, ctx)
            case jExpr: JExpr =>
              SExpr(jExpr, ctx)
            case IfStatement(cond, thenBranch, elseBranch) =>
              val sCond = SExpr.apply(cond, blockCtx)
              val sThen = apply(thenBranch, blockCtx)
          }
          statements += sTyped
      }
      new SBlock(statements.toIndexedSeq) withRangeOf block
    }
  }

  case class SBlock(parts: IndexedSeq[STyped]) extends STyped{
    override def sType: SType = parts.last.sType
  }

  case class SIf(condition: STyped, thenBranch: STyped, elseBranch: STyped) {
    requireType(condition, SBoolean)
  }


  trait SVar extends SExpr

  case class LocalVar(index: Int, name: String, sType: SType) extends SVar

  case class BlockContext(typeContext: TypeContext, localMap: Map[String, SVar], startIndex: Int,
                          thisType: SRefType, isStatic: Boolean){
    def isInstance = !isStatic

    def resolveType(jType: JType): SType = {
      typeContext.resolve(jType) match {
        case Some(t) => t
        case None => throw SemanticError("Can't resolve type", jType)
      }
    }

    def resolveVarOpt(tIdentifier: TIdentifier): Option[SVar] ={
      localMap.get(tIdentifier.data)
    }

    def resolveVar(tIdentifier: TIdentifier): SVar = {
      resolveVarOpt(tIdentifier) match{
        case Some(d) => d
        case None =>
          thisType.getField(tIdentifier.data) match{
            case Some(f) => f
            case None => throw SemanticError("Can't resolve identifier" ,tIdentifier)

          }
      }
    }

    def + (local: (String, SType)) = {
      val newVar = LocalVar(startIndex, local._1, local._2)
      copy(localMap = localMap+(newVar.name-> newVar), startIndex = startIndex + 1)
    }
  }

  sealed trait SExpr extends STyped{

  }

  object SExpr {


    case class SIntConst(data: Int) extends SExpr {
      override def sType: SType = SInt
    }

    case class SCharConst(data: Char) extends SExpr{
      override def sType: SType = SChar
    }

    case class SStringConst(data: String) extends SExpr{
      override def sType: SType = ExternalType.string
    }

    case class SBooleanConst(data: Boolean) extends SExpr{
      override def sType: SType = SBoolean
    }

    trait SBinary extends SExpr{
      def l: SExpr
      def r: SExpr
      requireType(l, sType)
      requireType(r, sType)
    }

    case class SIAdd(l: SExpr, r: SExpr) extends SBinary{
      override def sType: SType = SInt
    }

    case class SISub(l: SExpr, r: SExpr) extends SBinary{
      override def sType: SType = SInt
    }
    case class SIMul(l: SExpr, r: SExpr) extends SBinary{
      override def sType: SType = SInt
    }
    case class SIDiv(l: SExpr, r: SExpr) extends SBinary{
      override def sType: SType = SInt
    }

    case class SAssign(l: SExpr, r: SExpr) extends SExpr{
      requireType(r, l.sType)

      override def sType: SType = l.sType
    }

    import jmms.JToken._

    def apply(jExpr: JExpr, blockContext: BlockContext): SExpr = {
      val result = jExpr match {
        case JLiteral(l) => l match {
          case TInt(n) => SIntConst(n)
          case TChar(n) => SCharConst(n.head) // todo: handle char escape
          case TString(s) => SStringConst(s)
          case TReserve(JKeyword.k_true) => SBooleanConst(true)
          case TReserve(JKeyword.k_false) => SBooleanConst(false)
          case _ => throw SemanticError("Unresolved literal", jExpr)
        }
        case q: QualifiedIdent =>
          resolveQualified(q, blockContext) match {
            case Left(t) => throw SemanticError("Type appears in variable place", q)
            case Right(v) => v
          }
        case BinaryExpr(op, l ,r) =>
          val sl = apply(l, blockContext)
          val sr = apply(r, blockContext)
          op.data match {
            case "+" => SIAdd(sl, sr)
            case "-" => SISub(sl, sr)
            case "*" => SIMul(sl, sr)
            case "/" => SIDiv(sl, sr)
            case "=" => SAssign(sl, sr)
            case _ => throw SemanticError("Unresolved operator", op)
          }
        case MethodCall(f, args, isSuper) =>
          ???

      }
      result.withRangeOf(jExpr)
    }
  }

  def resolveQualified(qualifiedIdent: QualifiedIdent, blockContext: BlockContext): Either[SRefType,SVar] ={
    val parts = qualifiedIdent.parts
    val jExpr = qualifiedIdent
    if(parts.length == 1){
      val id = parts.head
      blockContext.resolveVarOpt(id).map(Right.apply).getOrElse{
        blockContext.thisType.getField(id.data) match {
          case Some(signature) if signature.isStatic || blockContext.isInstance =>
            Right(signature)
          case None => throw SemanticError("Can't resolve variable", jExpr)
        }
      }
    }else {
      val partial = ListBuffer[TIdentifier]()
      val left = parts.dropWhile(tk => {
        partial += tk
        blockContext.typeContext.resolve(QualifiedIdent(partial.toIndexedSeq)).isEmpty
      })
      val sType = blockContext.typeContext.resolve(QualifiedIdent(partial.toIndexedSeq)) match {
        case Some(t) => t
        case None => throw SemanticError("Can't resolve qualified", jExpr)
      }
      if(left.isEmpty){
        Left(sType)
      } else {
//        var primary: SVar = sType.getField(left.head)
//        left.foreach(tk => primary match {
//          case s: SRefType =>
//            s.getField(tk.data) match {
//              case Some(f) if f.isStatic => primary = f.sType
//              case None => throw SemanticError(s"Can't resolve static field $tk on $s", tk)
//            }
//          case other => throw SemanticError(s"Can't resolve static field $tk on $other", tk)
//        })
        throw SemanticError("Can't resolve static filed yet", left.head) //todo
      }
    }

  }
}