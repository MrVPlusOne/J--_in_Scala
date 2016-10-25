package jmms

import SemanticsParser._
import jmms.JToken.{TIdentifier, TReserve}
import jmms.SemanticsTree._
import jmms.SyntaxTree._
import jmms.SBasicType._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position


class SemanticsParser {

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
  def parseTypeContext(compilationUnit: CompilationUnit, context: TypeContext): TypeContext = {
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
      classDeclsToUse
    }

    // add classes defined in this file to fileContext
    fileContext ++= newClasses.map { dc => InternalType(pkgDotName, dc.name.data)}

    newClasses.foreach( dc => {
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
      dc.body.foreach{
        case (modifiers, member) =>
          val isStatic = modifiers.children.contains(TReserve(JKeyword.k_static))

          def resolveParams(formalParams: IndexedSeq[FormalParameter]): Option[IndexedSeq[SType]] = {
            Some(formalParams.map{
              case SyntaxTree.FormalParameter(jt, pName) =>
                fileContext.resolve(jt) match {
                  case None =>
                    newError("Can't resolve parameter type", jt)
                    return None
                  case Some(t) => t
                }
            })
          }

          member match {
            case FieldMemberDecl(VarDecl(FormalParameter(jt, n), _)) =>
              cl.getField(n.data) match {
                case Some(_) => newError(s"Field with name ${n.data} already defined", n)
                case None =>
                  fileContext.resolve(jt) match {
                    case Some(t) => cl.mkNewField(FieldSignature(n.data, t, isStatic))
                    case None => newError(s"Can't resolve filed type", jt)
                  }
              }
            case MethodMemberDecl(reJT, name, formalParams, _) =>
              resolveParams(formalParams).foreach { params =>
                if (cl.getMethod(name.data, params).isDefined) {
                  newError("Method with the same signature already defined", name)
                } else {
                  fileContext.resolve(reJT) match {
                    case None => newError("Can't resolve method return type", reJT)
                    case Some(rt) =>
                      cl.mkNewMethod(MethodSignature(name.data, params, rt, isStatic))
                  }
                }
              }

            case ConstructorDecl(name, formalParameters, _) =>
              if(name != dc.name){
                newError("method should have return type", name)
              }else resolveParams(formalParameters).foreach { params =>
                if(cl.getConstructor(params).isDefined)
                  newError("Constructor with the same signature already defined", name)
                else
                  cl.mkNewConstructor(ConstructorSignature(params))
              }
          }
      }
    })

    fileContext
  }

  def semanticAnalysis(compilationUnit: CompilationUnit, context: TypeContext): SemanticsTree = {
    ???
  }
}

object SemanticsParser{

  case class SemanticError(msg: String, parts: Seq[SyntaxTree]){
    def print(): Unit = {
      println(msg)
      parts.foreach(p => println(p.pos.longString))
    }
  }

  case class TypeContext(locals: Map[String, SRefType], loadExt: String => Option[SRefType]) {
    def resolve(qualifiedIdent: QualifiedIdent): Option[SRefType] = {
      val path = qualifiedIdent.toDotPath
      locals.get(path) match{
        case None => loadExt(path)
        case Some(t) => Some(t)
      }
    }

    def resolve(jType: JType): Option[SType] = {
      jType match {
        case JBasicType(TReserve(k)) => k match {
          case JKeyword.k_char => Some(SChar)
          case JKeyword.k_int => Some(SInt)
          case JKeyword.k_boolean => Some(SBoolean)
          case JKeyword.k_void => Some(SVoid)
          case _ => None
        }
        case BasicTypeArray(t: JBasicType, arrayDimensions: Int) =>
          for{
            b <- resolve(t)
            t <- loadExt("["*arrayDimensions + b.className)
          } yield t
        case RefTypeOrArray(q, dim) =>
          if(dim == 0)
            resolve(q)
          else{
            resolve(q).flatMap{ b =>
              loadExt("[" * dim + b.className)
            }
          }

      }
    }

    def +(s: SRefType) = {
      TypeContext(locals + (s.dotName -> s) + (s.simpleName -> s), loadExt)
    }

    def ++(ss: Seq[SRefType]) = {
      val newLocals = locals ++ ss.map(s => s.dotName -> s) ++ ss.map(s => s.simpleName -> s)
      TypeContext(newLocals, loadExt)
    }
  }

  object TypeContext{
    def empty() = TypeContext(Map(), name => {
      try {
        Some(ExternalType(Class.forName(name)))
      } catch {
        case _: ClassNotFoundException => None
      }
    })
  }

  def loadClassFromCurrentPath(name: String): Option[SRefType] = {
    try{
      Some(ExternalType(Class.forName(name)))
    } catch {
      case e: ClassNotFoundException => None
    }
  }
}