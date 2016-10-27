package jmms

import java.lang.reflect.Modifier

import cafebabe.ClassFileTypes.U2
import cafebabe.{CodeHandler, Flags}

/**
  * The common interface to represent j-- types
  */
sealed trait SType{
  def lDotName: String
  def queryName: String
  def simpleName: String

  def genTypeName = lDotName.replace('.', '/')
  def genName = queryName.replace('.', '/')

  /** return the lowest common type parent of this two types */
  def + (t2: SType): SType = this match {
    case SVoid => t2
    case SNoType => SNoType
    case b1: SBasicType =>
      if(t2 == b1) t2
      else if (t2 == SVoid) this
      else SNoType
    case r1: SRefType => t2 match {
      case SVoid => this
      case r2 : SRefType => SRefType.findCommon(r1, r2)
      case _ => SNoType
    }
  }
}

case object SNoType extends SType {
  override def lDotName: String = throw new Exception("No type")

  override def queryName: String = throw new Exception("No type")

  override def simpleName: String = "No type"
}

case object SVoid extends SType {
  override def lDotName: String = "V"

  override def queryName: String = throw new Exception("void")

  override def simpleName: String = "void"
}

trait SRefType extends SType{
  def isChildOf(t1: SRefType): Boolean = {
    if(this == t1) return true
    superClass match {
      case None => false
      case Some(s) => s.isChildOf(t1)
    }
  }

  def superClass: Option[SRefType]

  def getField(name: String): Option[FieldSignature]
  def getStaticFiled(name: String) = getField(name).filter(_.isStatic)
  def getInstanceFiled(name: String) = getField(name).filter(! _.isStatic)


  def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature]

  def getStaticMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] =
    getMethod(name, paramTypes).filter(_.isStatic)

  def getInstanceMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] =
    getMethod(name, paramTypes).filter(! _.isStatic)

  def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature]

//  def methods: Map[String,IndexedSeq[MethodSignature]]
//  def staticMethods: Map[String,IndexedSeq[MethodSignature]]

}

object SRefType{
  def findCommon(t1: SRefType, t2: SRefType): SRefType = {
    if(t1.isChildOf(t2)) t2
    else t2.superClass match {
      case Some(s) => findCommon(t1, s)
      case None => throw new Exception(s"Ref type no parent! $t2")
    }
  }
}

sealed trait SBasicType extends SType{
  override def simpleName = queryName
}

object SBasicType{
  case object SInt extends SBasicType {
    override def lDotName: String = "I"

    override def queryName: String = "int"
  }

  case object SChar extends SBasicType {
    override def lDotName: String = "C"

    override def queryName: String = "char"
  }

  case object SBoolean extends SBasicType{
    override def lDotName: String = "Z"

    override def queryName: String = "boolean"
  }
}

case class ExternalType (repr: Class[_]) extends SRefType{

  override def queryName: String = repr.getName

  override def simpleName: String = repr.getSimpleName

  override def lDotName: String = {
    if(repr.getName == "void")
      return "V"

    val aug = if (queryName.startsWith("[")) queryName else s"L$queryName;"
    aug
  }

  override def superClass: Option[SRefType] = repr.getSuperclass match{
    case null => None
    case s => Some(ExternalType(s))
  }

  override def getField(name: String): Option[FieldSignature] = {
    try{
      val f = repr.getField(name)
      val isStatic = (f.getModifiers & Modifier.STATIC) != 0
      Some(FieldSignature(name, this, ExternalType(f.getType), isStatic))
    }catch {
      case _: Exception => None
    }
  }

  override def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature] = {
    try {
      val ps = paramTypes.map (_.asInstanceOf[ExternalType].repr)
      val cons = repr.getConstructor(ps: _*)
      Some(ConstructorSignature(this, paramTypes))
    } catch {
      case _: Exception => None
    }
  }

  override def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] = {
    try {
      val ps = paramTypes.map (p => Class.forName(p.queryName))
      val m = repr.getMethod(name, ps: _*)
      val isStatic = (m.getModifiers & Modifier.STATIC) != 0
      val params = m.getParameterTypes.toIndexedSeq.map(t => ExternalType(t))
      val rt = ExternalType(m.getReturnType)
      Some(MethodSignature(name, this, params, rt, isStatic))
    } catch {
      case _: Exception => None
    }
  }

  override def toString: String = s"t'$repr'"
}


object ExternalType{
  val obj = ExternalType(Class.forName("java.lang.Object"))
  val string = ExternalType(Class.forName("java.lang.String"))
  val system = ExternalType(Class.forName("java.lang.System"))

  def create(repr: Class[_]) = {
    if(repr.getName == "void")
      SVoid
    else ExternalType(repr)
  }
}

case class FieldSignature(name: String, classType: SRefType, fieldType: SType, isStatic: Boolean) {
}


case class MethodSignature(name: String, classType: SRefType, args: IndexedSeq[SType], returns: SType, isStatic: Boolean) {
  def flags: U2 = {
    var f = 0
    f = f | Flags.METHOD_ACC_PUBLIC
    if(isStatic)
      f = f | Flags.METHOD_ACC_STATIC

    f.toShort
  }

  def signatureString: String = {
    "("+args.map(_.genTypeName).mkString("")+s")${returns.genTypeName}"
  }

  def fullCodeName = classType.queryName.replace('.','/') + "/" + name
}

case class ConstructorSignature(classType: SRefType, args: IndexedSeq[SType])