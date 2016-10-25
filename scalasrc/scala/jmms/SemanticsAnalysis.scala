package jmms

import jmms.JToken.{TIdentifier, TReserve}
import jmms.SBasicType._
import jmms.STyped.{BlockContext, SBlock}
import jmms.SemanticsAnalysis._
import jmms.SemanticsTree._
import jmms.SyntaxTree._

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
      dc.body.foreach{
        case (modifiers, member) =>
          val isStatic = modifiers.children.contains(TReserve(JKeyword.k_static))
          val isAbstract = modifiers.children.contains(TReserve(JKeyword.k_abstract))

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
              resolveParams(formalParams, fileContext).foreach { params =>
                if (cl.getMethod(name.data, params).isDefined) {
                  newError("Method with the same signature already defined", name)
                } else {
                  fileContext.resolve(reJT) match {
                    case None => newError("Can't resolve method return type", reJT)
                    case Some(rt) =>
                      if(! isAbstract && body.isEmpty)
                        newError("non-abstract method must have a body", member)
                      val method = InternalMethod(
                        MethodSignature(name.data, params, rt, isStatic),
                        body
                      )
                      cl.mkNewMethod(method)
                  }
                }
              }

            case ConstructorDecl(name, formalParameters, _) =>
              if(name != dc.name){
                newError("method should have return type", name)
              }else resolveParams(formalParameters, fileContext).foreach { params =>
                if(cl.getConstructor(params).isDefined)
                  newError("Constructor with the same signature already defined", name)
                else
                  cl.mkNewConstructor(InternalConstructor(ConstructorSignature(params)))
              }
          }
      }
      cl
    })

    SPackage(classes, fileContext)
  }

  def resolveParams(formalParams: IndexedSeq[FormalParameter], typeContext: TypeContext): Option[IndexedSeq[SType]] = {
    Some(formalParams.map{
      case SyntaxTree.FormalParameter(jt, pName) =>
        typeContext.resolve(jt) match {
          case None =>
            newError("Can't resolve parameter type", jt)
            return None
          case Some(t) => t
        }
    })
  }

  def fullyAnalyze(sPackage: SPackage) {
    val typeContext = sPackage.typeContext
    sPackage.classes.foreach(ty => {
      ty.localStaticMethods.values.foreach(sm => {
        sm.sBlock = try {
          sm.impl.map{ jBlock =>
            SBlock.apply(jBlock, BlockContext(typeContext, Map(), 0, thisType = ty , isStatic = false))
          }
        } catch {
          case e: SemanticError =>
            errorList += e
            None
        }
      })
    })
  }


}

object SemanticsAnalysis{

  case class SemanticError(msg: String, parts: Seq[Ranged]) extends Throwable{
    def print(): Unit = {
      println(msg)
      parts.foreach(p => println(p.pos.longString))
    }
  }

  object SemanticError{
    def apply(msg: String, part1: Ranged, parts: Ranged*): SemanticError = {
      new SemanticError(msg, part1 +: parts)
    }
  }


  def loadClassFromCurrentPath(name: String): Option[SRefType] = {
    try{
      Some(ExternalType(Class.forName(name)))
    } catch {
      case e: ClassNotFoundException => None
    }
  }
}