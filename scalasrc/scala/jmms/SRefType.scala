package jmms

import java.lang.reflect.Modifier

/**
  * The common interface to represent j-- types
  */
sealed trait SType{
  def className: String
}

trait SRefType extends SType{
  def isChildOf(t1: SRefType): Boolean = {
    if(this == t1) return true
    superClass match {
      case None => false
      case Some(s) => s.isChildOf(t1)
    }
  }

  def simpleName: String
  def dotName: String

  override def className: String = s"L$dotName;"

  def superClass: Option[SRefType]

  def getField(name: String): Option[FieldSignature]
  def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature]
  def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature]

//  def methods: Map[String,IndexedSeq[MethodSignature]]
//  def staticMethods: Map[String,IndexedSeq[MethodSignature]]

}

sealed trait SBasicType extends SType{

}

object SBasicType{
  case object SInt extends SBasicType {
    override def className: String = "I"
  }

  case object SChar extends SBasicType {
    override def className: String = "C"
  }

  case object SBoolean extends SBasicType{
    override def className: String = "Z"
  }

  case object SVoid extends SBasicType{
    override def className: String = "V"
  }
}



case class ExternalType(repr: Class[_]) extends SRefType{
  override def dotName: String = repr.getName

  override def simpleName: String = repr.getSimpleName

  override def superClass: Option[SRefType] = repr.getSuperclass match{
    case null => None
    case s => Some(ExternalType(s))
  }

  override def getField(name: String): Option[FieldSignature] = {
    try{
      val f = repr.getField(name)
      val isStatic = (f.getModifiers & Modifier.STATIC) != 0
      Some(FieldSignature(name, ExternalType(f.getType), isStatic))
    }catch {
      case _: Exception => None
    }
  }

  override def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature] = {
    try {
      val ps = paramTypes.map (_.asInstanceOf[ExternalType].repr)
      val cons = repr.getConstructor(ps: _*)
      Some(ConstructorSignature(paramTypes))
    } catch {
      case _: Exception => None
    }
  }

  override def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] = {
    try {
      val ps = paramTypes.map (_.asInstanceOf[ExternalType].repr)
      val m = repr.getMethod(name, ps: _*)
      val isStatic = (m.getModifiers & Modifier.STATIC) != 0
      val params = m.getParameterTypes.toIndexedSeq.map(t => ExternalType(t))
      val rt = ExternalType(m.getReturnType)
      Some(MethodSignature(name, params, rt, isStatic))
    } catch {
      case _: Exception => None
    }
  }

  override def toString: String = s"t'$repr'"
}


object ExternalType{
  val obj = ExternalType(Class.forName("java.lang.Object"))
  val string = ExternalType(Class.forName("java.lang.String"))
}


case class FieldSignature(name: String, tpe: SType, isStatic: Boolean)

case class MethodSignature(name: String, args: IndexedSeq[SType], returns: SType, isStatic: Boolean) {

}

case class ConstructorSignature(args: IndexedSeq[SType])