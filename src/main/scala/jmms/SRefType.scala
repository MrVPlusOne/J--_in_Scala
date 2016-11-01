package jmms

import java.lang.reflect.Modifier

import cafebabe.ClassFileTypes.U2
import cafebabe.{CodeHandler, Flags}
import jmms.SBasicType.{SBoolean, SInt, SVoid}
import jmms.SemanticTree.InternalType

/**
  * The common interface to represent j-- types
  */
sealed trait SType{
  def javaExtendedName: String
  def javaSimpleName: String

  def simpleName: String

  def jvmTypeName = javaExtendedName.replace('.', '/')
  def jvmClassName = javaSimpleName.replace('.', '/')

  def isArrayType = jvmTypeName.startsWith("[")

  /** return the lowest common type parent of this two types */
  def + (that: SType): SType = this match {
    case SNoType => SNoType
    case b1: SBasicType =>
      if(that == b1) that
      else if (that == SVoid) this
      else if (this == SVoid) that
      else SNoType
    case r1: SRefType => that match {
      case SVoid => this
      case r2 : SRefType => SRefType.findCommon(r1, r2)
      case _ => SNoType
    }
  }
}

case object SNoType extends SType {
  override def javaExtendedName: String = throw new Exception("No type")

  override def javaSimpleName: String = throw new Exception("No type")

  override def simpleName: String = "No type"

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

  def arrayDimAndElemType: (Int, SType)

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
  override def javaSimpleName: String = simpleName

  def externalRepr: Class[_]
}

object SBasicType{
  case object SVoid extends SBasicType {
    override def simpleName: String = "void"

    override def javaExtendedName: String = "V"

    override def externalRepr: Class[_] = classOf[Unit]
  }

  case object SInt extends SBasicType {

    override def simpleName: String = "int"

    override def javaExtendedName: String = "I"

    override def externalRepr: Class[_] = classOf[Int]
  }

  case object SChar extends SBasicType {
    override def simpleName: String = "char"

    override def javaExtendedName: String = "C"

    override def externalRepr: Class[_] = classOf[Char]
  }

  case object SBoolean extends SBasicType{

    override def javaExtendedName: String = "Z"

    override def simpleName: String = "boolean"

    override def externalRepr: Class[_] = classOf[Boolean]
  }

  val basicTypes = Set(SVoid, SInt, SChar, SBoolean)
  val javaExtendedNameMap = basicTypes.map(t => t.javaExtendedName -> t).toMap
  val javaSimpleNameMap = basicTypes.map(t => t.javaSimpleName -> t).toMap
}

case class SArray(tp: InternalType, dim: Int) extends SRefType{
  require(dim>0)
  override def superClass: Option[SRefType] = Some(ExternalType.obj)

  def elementType = if(dim == 1) tp else SArray(tp, dim-1)

  override def arrayDimAndElemType: (Int, SType) = (dim, tp)

  override def getField(name: String): Option[FieldSignature] = name match {
    case "length" => Some(FieldSignature("length", this, elementType, isStatic = false))
    case _ => None
  }

  override def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] = None

  override def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature] = ???

  override def javaExtendedName: String = "[" * dim + tp.javaExtendedName

  override def javaSimpleName: String = "[" * dim + tp.javaSimpleName

  override def simpleName: String = s"${tp.simpleName}${"[]" * dim}"
}

class ExternalType private (private val repr: Class[_]) extends SRefType {
  assert(SBasicType.javaSimpleNameMap.get(repr.getName).isEmpty)

  override def equals(obj: scala.Any): Boolean = obj match {
    case st: SType => st.javaSimpleName == this.javaSimpleName
    case _ => false
  }


  override def arrayDimAndElemType: (Int, SType) = {
    var dim = 0
    var elem = repr
    while(elem.getComponentType != null){
      dim += 1
      elem = elem.getComponentType
    }
    (dim, ExternalType(elem))
  }

  override def javaSimpleName: String = repr.getName

  override def simpleName: String = repr.getSimpleName

  override def javaExtendedName: String = {
    val aug = if (javaSimpleName.startsWith("[")) javaSimpleName else s"L$javaSimpleName;"
    aug
  }

  override def superClass: Option[SRefType] = repr.getSuperclass match{
    case null => None
    case s => Some(new ExternalType(s))
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

  def paramsToExtRepr(paramTypes: IndexedSeq[SType]): IndexedSeq[Class[_]] = {
    paramTypes.map{
      case b: SBasicType => b.externalRepr
      case ext: ExternalType => ext.repr
    }
  }

  override def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature] = {
    try {
      val ps = paramsToExtRepr(paramTypes)
      val cons = repr.getConstructor(ps: _*)
      Some(ConstructorSignature(this, paramTypes))
    } catch {
      case _: Exception => None
    }
  }

  override def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] = {
    try {
      val ps = paramsToExtRepr(paramTypes)
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
  val obj = new ExternalType(Class.forName("java.lang.Object"))
  val string = new ExternalType(Class.forName("java.lang.String"))
  val system = new ExternalType(Class.forName("java.lang.System"))
  val integer = new ExternalType(Class.forName("java.lang.Integer"))
  val char = new ExternalType(Class.forName("java.lang.Character"))
  val boolean = new ExternalType(Class.forName("java.lang.Boolean"))

  def predef = Seq(obj, string, system, integer, char, boolean)

  def apply(repr: Class[_]) = {
    SBasicType.javaSimpleNameMap.get(repr.getName) match{
      case Some(t) => t
      case None => new ExternalType(repr)
    }
  }
}

case class FieldSignature(name: String, classType: SRefType, fieldType: SType, isStatic: Boolean) {
  def flags: U2 = {
    var f = 0
    f = f | Flags.FIELD_ACC_PUBLIC
    if(isStatic)
      f = f | Flags.FIELD_ACC_STATIC
    f.toShort
  }
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
    "("+args.map(_.jvmTypeName).mkString("")+s")${returns.jvmTypeName}"
  }

  def fullCodeName = classType.javaSimpleName.replace('.','/') + "/" + name
}

case class ConstructorSignature(classType: SRefType, args: IndexedSeq[SType]){
  def flags: U2 = Flags.METHOD_ACC_PUBLIC
}

object ConstructorSignature{
  def signatureString(args: IndexedSeq[SType]) = {
    "("+args.map(_.jvmTypeName).mkString("")+s")V"
  }
}