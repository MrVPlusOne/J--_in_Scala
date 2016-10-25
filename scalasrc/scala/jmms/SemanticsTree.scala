package jmms

import jmms.STyped.SBlock
import jmms.SyntaxTree._

/**
  * Semantics-level representation of the program
  */
sealed trait SemanticsTree {

}

object SemanticsTree {

  case class SPackage(classes: IndexedSeq[InternalType], typeContext: TypeContext) extends SemanticsTree{
  }

  case class InternalType(pkg: String, simpleName: String, isAbstract: Boolean) extends SemanticsTree with SRefType {

    override val dotName: String = pkg + "." + simpleName

    var superClass: Option[SRefType] = None

    var localStaticFields: Map[String, InternalField] = Map()

    var localInstanceFields: Map[String, InternalField] = Map()

    var localInstanceMethods: Map[(String, IndexedSeq[SType]),InternalMethod] = Map()

    var localStaticMethods: Map[(String, IndexedSeq[SType]),InternalMethod] = Map()

    var constructors: Map[IndexedSeq[SType], InternalConstructor] = Map()

    def mkNewField(field: InternalField): Unit = {
      val signature = field.signature
      val m = signature.name -> field
      if (signature.isStatic) localStaticFields += m
      else localInstanceFields += m
    }

    def mkNewMethod(method: InternalMethod): Unit = {
      val signature = method.signature
      val m = (signature.name, signature.args) -> method
      if(signature.isStatic) localStaticMethods += m
      else localInstanceMethods += m
    }

    def mkNewConstructor(cs: InternalConstructor): Unit = {
      val signature = cs.signature
      constructors += signature.args -> cs
    }

    override def toString: String = {
      s"class $dotName extends $superClass"
    }

    override def getField(name: String): Option[FieldSignature] = {
      localStaticFields.get(name).map(_.signature).
        orElse(localInstanceFields.get(name).map(_.signature)).
        orElse(superClass.flatMap(_.getField(name)))
    }

    override def getMethod(name: String, paramTypes: IndexedSeq[SType]): Option[MethodSignature] = {
      val key = name -> paramTypes
      localStaticMethods.get(key).map(_.signature).
        orElse(localInstanceMethods.get(key).map(_.signature)).
        orElse(superClass.flatMap(_.getMethod(name, paramTypes)))
    }

    override def getConstructor(paramTypes: IndexedSeq[SType]): Option[ConstructorSignature] = {
      constructors.get(paramTypes).map(_.signature)
    }
  }

  case class InternalMethod(signature: MethodSignature, impl: Option[Block]) extends SemanticsTree {
    var sBlock: Option[SBlock] = None
  }


  case class InternalField(signature: FieldSignature) extends SemanticsTree {

  }

  case class InternalConstructor(signature: ConstructorSignature) extends SemanticsTree{

  }

  case class SField()

}