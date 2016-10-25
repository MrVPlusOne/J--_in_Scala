package jmms

/**
  * Semantics-level representation of the program
  */
sealed trait SemanticsTree {

}

object SemanticsTree {

  case class SPackage(classes: InternalType) extends SemanticsTree{
  }

  case class InternalType(pkg: String, simpleName: String) extends SemanticsTree with SRefType {

    override val dotName: String = pkg + "." + simpleName

    var superClass: Option[SRefType] = None

    var localStaticFields: Map[String, InternalField] = Map()

    var localInstanceFields: Map[String, InternalField] = Map()

    var localInstanceMethods: Map[(String, IndexedSeq[SType]),InternalMethod] = Map()

    var localStaticMethods: Map[(String, IndexedSeq[SType]),InternalMethod] = Map()

    var constructors: Map[IndexedSeq[SType], InternalConstructor] = Map()

    def mkNewField(signature: FieldSignature): Unit = {
      val m = signature.name -> InternalField(signature)
      if (signature.isStatic) localStaticFields += m
      else localInstanceFields += m
    }

    def mkNewMethod(signature: MethodSignature): Unit = {
      val m = (signature.name, signature.args) -> InternalMethod(signature)
      if(signature.isStatic) localStaticMethods += m
      else localInstanceMethods += m
    }

    def mkNewConstructor(signature: ConstructorSignature): Unit ={
      constructors += signature.args -> InternalConstructor(signature)
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

  case class InternalMethod(signature: MethodSignature) extends SemanticsTree {

  }

  case class InternalField(signature: FieldSignature) extends SemanticsTree {

  }

  case class InternalConstructor(signature: ConstructorSignature) extends SemanticsTree{

  }

  case class SField()
}