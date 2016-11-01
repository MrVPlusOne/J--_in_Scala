package jmms

import cafebabe.AbstractByteCodes
import jmms.JToken.{TIdentifier, TReserve}
import jmms.SBasicType._
import jmms.SemanticTree.SExpr._
import jmms.SemanticTree.{BlockContext, SBlock}
import jmms.SemanticsAnalysis._
import jmms.SemanticTree._
import jmms.SyntaxTree._
import jmms.SyntaxTree.JExpr._

import scala.collection.mutable.ListBuffer


class SemanticsAnalysis {

  private val errorList = ListBuffer[SemanticError]()

  def newError(msg: String, part0: SyntaxTree, parts: SyntaxTree*): Unit = {
    errorList += SemanticError(msg, part0 +: parts)
  }

  def clearErrors() = errorList.clear()

  def currentErrors() = errorList.toList

  /** find out all the imported and newly defined classes in this `CompilationUnit`,
    * then incorporate them into the `TypeContext` provided, and return the new context.
    * Methods declarations are also added to the `InternalType`s created.
    */
  def analyzeTypeContext(compilationUnit: CompilationUnit, context: TypeContext): SPackage = {
    val pkgDotName = compilationUnit.packageDeclaration.toDotPath
    var fileContext = compilationUnit.imports.foldLeft(context){
      case (ctx, im) => ctx.resolve(im) match{
        case Some(t) => ctx + t
        case None =>
          newError(s"Can't resolve import", im)
          ctx // ignore unresolved one in later stages
      }
    }

    // find out all classes defined in this package.
    val newClasses = {
      var classDeclsToUse = ListBuffer[ClassDecl]()
      var classNames = Set[TIdentifier]()
      compilationUnit.classDecls.foreach { dc =>
        if (classNames contains dc.name) {
          newError("Class name already defined", dc.name) // ignore the duplicated one in later stages
        } else {
          classNames += dc.name
          classDeclsToUse += dc
        }
      }
      classDeclsToUse.toIndexedSeq
    }

    // add classes defined in this file to fileContext
    fileContext ++= newClasses.map { dc =>
      InternalType(pkgDotName, dc.name.data, dc.modifiers.isAbstract)
    }

    val classes = newClasses.map( dc => {
      val cl = fileContext.resolve(QualifiedIdent(dc.name)).get.asInstanceOf[InternalType]

      // set up superclasses
      val superClass = dc.inheritance match {
        case None => ExternalType.obj
        case Some(q) => fileContext.resolve(q) match{
          case Some(t) =>
            if(!t.isChildOf(cl)) t
            else {
              newError("Circular inheritance detected", q)
              ExternalType.obj
            }
          case None =>
            newError("Inherited from unresolved Class", q)
            ExternalType.obj
        }
      }
      cl.superClass = Some(superClass)

      // parse members
      dc.body.foreach {
        case (modifiers, member) =>
          val isStatic = modifiers.children.contains(TReserve(JKeyword.k_static))
          val isAbstract = modifiers.children.contains(TReserve(JKeyword.k_abstract))

          try {
            member match {
              case FieldMemberDecl(VarDecl(FormalParameter(jt, n), initializer)) =>
                cl.getField(n.data) match {
                  case Some(_) => newError(s"Field with name ${n.data} already defined", n)
                  case None =>
                    fileContext.resolve(jt) match {
                      case Some(t) =>
                        val field = InternalField(FieldSignature(n.data, cl, t, isStatic))
                        cl.mkNewField(field)
                      case None => newError(s"Can't resolve filed type", jt)
                    }
                }
              case MethodMemberDecl(reJT, name, formalParams, body) =>
                val params = resolveParams(formalParams, fileContext)

                if (cl.getMethod(name.data, params).isDefined) {
                  newError("Method with the same signature already defined", name)
                } else {
                  fileContext.resolve(reJT) match {
                    case None => newError("Can't resolve method return type", reJT)
                    case Some(rt) =>
                      if (!isAbstract && body.isEmpty)
                        newError("non-abstract method must have a body", member)
                      val method = InternalMethod(
                        MethodSignature(name.data, cl, params, rt, isStatic),
                        formalParams.map(_.identifier),
                        body
                      )
                      cl.mkNewMethod(method)
                  }
                }
              case ConstructorDecl(name, formalParameters, body) =>
                if (name != dc.name) {
                  newError("method should have return type", name)
                } else {
                  val params = resolveParams(formalParameters, fileContext)
                  if (cl.getConstructor(params).isDefined)
                    newError("Constructor with the same signature already defined", name)
                  else {
                    cl.mkNewConstructor(InternalConstructor(ConstructorSignature(cl, params),
                      formalParameters.map(_.identifier), body))
                  }
                }
            }
          } catch {
            case e: SemanticError => errorList += e
          }
      }
      cl
    })

    SPackage(classes, fileContext)
  }


  def fullyAnalyze(sPackage: SPackage) {
    val typeContext = sPackage.typeContext

    def methodSBlock(args: IndexedSeq[SType], argNames: IndexedSeq[TIdentifier], returnType: SType, impl: Option[Block],
                       isStatic: Boolean, thisClass: SRefType): Option[SBlock] = {
      val startIndex = if (isStatic) 0 else 1
      var initContext = BlockContext(typeContext, Map(), startIndex, thisType = thisClass, isStatic = false)
      args.zipWithIndex.foreach {
        case (t, i) =>
          initContext += initContext.createLocalVar(argNames(i).data, t, argNames(i))
      }
      try {
        impl.map { jBlock =>
          val b = pBlock(
            jBlock,
            initContext)
          requireReturnType(b, returnType)
          b
        }
      } catch {
        case e: SemanticError =>
          errorList += e
          None
      }
    }

    sPackage.classes.foreach(cl => {
      cl.allMethods.foreach(sm => {
        val sig = sm.signature
        sm.sBlock = methodSBlock(sig.args, sm.paramNames, sig.returns, sm.impl, sig.isStatic, sig.classType)
      })

      cl.constructors.values.foreach(cn => {
        val sig = cn.signature
        methodSBlock(sig.args, cn.paramNames, SVoid, Some(cn.impl), isStatic = false, sig.classType).foreach(cn.sBlock = _)
      })
    })

  }

  def pBlock(block: Block, originalCtx: BlockContext): SBlock = {
    var ctx = originalCtx
    var newVars = IndexedSeq[SLocalVar]()
    var statements = ListBuffer[SemanticStatement]()
    block.children.foreach{
      case d @ LocalVarDecl(VarDecl(formalParameter, initializer)) =>
        val sType = ctx.resolveType(formalParameter.jType)
        val v = ctx.createLocalVar(formalParameter.identifier.data, sType, d)
        newVars :+= v
        ctx += v
        initializer.foreach{
          case ExprInit(expr) =>
            statements += ExprStatement(SAssign(v, pExpr(expr, ctx)))
          case ArrayInit(varInits) => varInits.zipWithIndex.foreach {
            case (ExprInit(expr), idx) =>
              statements += ExprStatement(SArrayStore(v, SIntConst(idx), pExpr(expr, ctx)))
            case _ => ??? // nested array init unsupported yet
          }
        }
      case s: Statement =>
        val sTyped: SemanticStatement = s match {
          case b1: Block =>
            pBlock(b1, ctx)
          case jExpr: JExpr =>
            ExprStatement(pExpr(jExpr, ctx))
          case IfStatement(cond, thenBranch, elseBranch) =>
            val sCond = pExpr(cond, ctx)
            val sThen = pBlock(thenBranch, ctx)
            val sElse = elseBranch.map(e => pBlock(e, ctx))
            SIf(sCond, sThen, sElse)
          case WhileStatement(cond, body) =>
            val sCond = pExpr(cond, ctx)
            val sBody = pBlock(body, ctx)
            SWhile(sCond, sBody)
          case ReturnStatement(expr) =>
            val se = expr.map(e => pExpr(e, ctx))
            SReturn(se)
          case _: LocalVarDecl => throw new Exception("Impossible!")
        }
        statements += sTyped
    }
    SBlock(statements.toIndexedSeq, newVars) withRangeOf block
  }

  import jmms.JToken._

  def pExpr(jExpr: JExpr, blockContext: BlockContext): SExpr = {
    val result = jExpr match {
      case JLiteral(l) => l match {
        case TInt(n) => SIntConst(n)
        case TChar(n) => SCharConst(n.head) // todo: handle char escape
        case TString(s) => SStringConst(s)
        case TReserve(JKeyword.k_true) => SBooleanConst(true)
        case TReserve(JKeyword.k_false) => SBooleanConst(false)
        case _ => throw SemanticError("Unresolved literal", jExpr)

      }
      case JThis => blockContext.contextThis.getOrElse{
        throw SemanticError("Can't access 'this' from within static methods", jExpr)}
      case q: QualifiedIdent =>
        blockContext.resolveQualified(q.parts) match {
          case Left(t) => throw SemanticError("Type appears in variable place", q)
          case Right(v) => v
        }
      case BinaryExpr(op, l ,r) =>
        val sl = pExpr(l, blockContext)
        val sr = pExpr(r, blockContext)

        def stringConcat(sl: SExpr, sr: SExpr) = {
          if(sl.exprType == sr.exprType && sl.exprType == SInt) SIAdd(sl, sr)
          else SStringConcat(SConvertString.convert(sl), SConvertString.convert(sr))
        }

        def assignLeft(sExpr: SExpr) = SAssign(sl, sExpr)

        op.data match {
          case "+" => stringConcat(sl,sr)
          case "+=" => assignLeft(stringConcat(sl, sr))
          case "-" => SISub(sl, sr)
          case "-=" =>  assignLeft(SISub(sl, sr))
          case "*" => SIMul(sl, sr)
          case "*=" => assignLeft(SIMul(sl, sr))
          case "/" => SIDiv(sl, sr)
          case "/=" => assignLeft(SIDiv(sl, sr))
          case "=" => SAssign(sl, sr)
          case "<" => SBinaryIntComp(sl, sr, AbstractByteCodes.If_ICmpLt)
          case "<=" => SBinaryIntComp(sl, sr, AbstractByteCodes.If_ICmpLe)
          case "==" => SBinaryIntComp(sl, sr, AbstractByteCodes.If_ICmpEq)
          case ">" => SBinaryIntComp(sl, sr, AbstractByteCodes.If_ICmpGt)
          case ">=" => SBinaryIntComp(sl, sr, AbstractByteCodes.If_ICmpGe)
          case _ => throw SemanticError("Unresolved operator", op)
        }
      case u: UnaryExpr =>
        val sExpr = pExpr(u.expr, blockContext)
        u match {
          case _: UnaryNegate => SUnaryNegate(sExpr)
          case _: UnaryNot => SUnaryNot(sExpr)
          case BasicCast(basicType: JBasicType, _) =>
            ???
          case RefTypeCast(refType: JType, _) =>
            val t = blockContext.resolveType(refType)
            SCast(sExpr, t)

        }
      case withArgs: JArgsExpr =>
        val (sArgs, argTypes) = resolveArguments(withArgs.args, blockContext)
        withArgs match {
          case MethodCall(f, _, isSuper) =>
            if(f.parts.length == 1){
              val fName = f.parts.head
              val m = blockContext.thisType.getMethod(fName.data, argTypes).getOrElse{
                throw SemanticError(s"Can't resolve method $fName with such signature", f)
              }
              if(m.isStatic)
                StaticMethodCall(m, sArgs)
              else {
                InstanceMethodCall(blockContext.contextThis.getOrElse{
                  throw SemanticError("Can't call instance method within a static method", f)},
                  m, sArgs)
              }
            } else {
              val mName = f.parts.last.data
              blockContext.resolveQualified(f.parts.init) match {
                case Left(t) =>
                  val method = t.getStaticMethod(mName, argTypes).getOrElse{
                    throw SemanticError(s"Can't resolve static method $mName with such signature",f.parts.last)
                  }
                  StaticMethodCall(method, sArgs)
                case Right(v) => v.exprType match {
                  case ref: SRefType =>
                    val method = ref.getInstanceMethod(mName, argTypes).getOrElse {
                      throw SemanticError(s"Can't resolve instance method $mName with such signature", f.parts.last)
                    }
                    InstanceMethodCall(v, method, sArgs)
                  case _ => throw SemanticError(s"Can't resolve instance method $mName on value type $v", v)
                }
              }
            }

          case JCreator(jType, _) =>
            val creatType = blockContext.resolveType(jType)
            SCreator(creatType, sArgs)

          case c@ConstructorCall(args, isSuper) =>
            val callOn = if(isSuper) {
              blockContext.thisType.superClass.getOrElse(throw SemanticError("Super class not exist", c))
            } else blockContext.thisType
            val cons = callOn.getConstructor(argTypes).getOrElse(throw SemanticError(s"Can't resolve constructor of this signature", c))
            SConstructorCall(
              blockContext.contextThis.getOrElse(throw SemanticError("Can't call constructor here", c)),
              cons, sArgs
            )

        }

      case PostExpr(body, selectors) =>
        val sBody = pExpr(body, blockContext)
        selectors.foldLeft(sBody){
          case (acc, selector) => selector match {
            case JQualifiedSelector(ident, argsOpt) =>
              argsOpt match {
                case Some(jArguments) =>
                  val mName = ident.parts.last
                  val callOn = blockContext.fieldSelectionChain(acc, ident.parts.init)
                  callOn.exprType match {
                    case ref: SRefType =>
                      val (sArgs, argTyps) = resolveArguments(jArguments, blockContext)
                      val sig = ref.getInstanceMethod(mName.data, argTyps).getOrElse(throw SemanticError(s"Can't resolve instance method ${mName.data} with such signature $jArguments", jArguments))
                      InstanceMethodCall(callOn, sig, sArgs)
                    case _ => throw SemanticError(s"Can't resolve instance method $mName on value $callOn", callOn)
                  }
                case None =>
                  blockContext.fieldSelectionChain(acc, ident.parts)
              }
            case js@JArraySelector(idx) =>
              val sIdx = pExpr(idx, blockContext)
              val elemType = acc.exprType match{
                case ext: ExternalType if ext.isArrayType =>
                  val tName = ext.javaExtendedName.drop(1)
                  SBasicType.javaExtendedNameMap.get(tName).
                    orElse(blockContext.typeContext.loadExt(tName)).
                    getOrElse(throw new Exception(s"can't load external type: $tName"))
                case sa: SArray => sa.elementType
                case t => throw SemanticError(s"$t is not an array", js)
              }
              SArrayAccess(acc, sIdx, elemType)
          }
        }
    }
    result.withRangeOf(jExpr)
  }

  def resolveArguments(jArguments: JArguments, blockContext: BlockContext) = {
    val sArgs = jArguments.args.map(jExpr => pExpr(jExpr, blockContext))
    val argTypes = sArgs.map(_.exprType)
    (sArgs, argTypes)
  }

}

object SemanticsAnalysis{

  def fullyAnalyze(code: String, sourceName: String = "Unspecified") = {
    val semanP = new SemanticsAnalysis()
    val r = SyntaxParser.parseString(SyntaxParser.pCompilationUnit, code, sourceName)

    val sPackage = semanP.analyzeTypeContext(r, TypeContext.default())
    semanP.fullyAnalyze(sPackage)
    semanP.currentErrors().foreach(_.print())
    require(semanP.currentErrors().isEmpty, s"should be no analyzing error in $sourceName")
    sPackage
  }

  case class SemanticError(msg: String, parts: Seq[Ranged]) extends Throwable{
    def print(): Unit = {
      println(msg)
      println(s"  (On part: ${parts.mkString(" and ")})")
      parts.foreach(p => println(p.pos.longString))
    }
  }

  object SemanticError{
    def apply(msg: String, part1: Ranged, parts: Ranged*): SemanticError = {
      new SemanticError(msg, part1 +: parts)
    }
  }


  def resolveParams(formalParams: IndexedSeq[FormalParameter], typeContext: TypeContext): IndexedSeq[SType] = {
    formalParams.map{
      case SyntaxTree.FormalParameter(jt, pName) =>
        typeContext.resolve(jt) match {
          case None =>
            throw SemanticError("Can't resolve parameter type", jt)
          case Some(t) => t
        }
    }
  }

}