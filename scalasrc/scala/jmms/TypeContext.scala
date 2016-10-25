package jmms

import jmms.JToken.TReserve
import jmms.SBasicType.{SBoolean, SChar, SInt, SVoid}
import jmms.SemanticsTree.InternalType
import jmms.SyntaxTree._

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