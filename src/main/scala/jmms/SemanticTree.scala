package jmms

import cafebabe.{AbstractByteCodes => AC}
import cafebabe.ByteCodes.ByteCode
import cafebabe.{ClassFile, CodeHandler, Flags, MethodHandler, ByteCodes => BC}
import jmms.JToken.TIdentifier
import jmms.SemanticsAnalysis.SemanticError
import jmms.SyntaxTree._
import jmms.SBasicType._
import jmms.SemanticTree.SExpr.{SInstanceFieldAccess, StaticFieldAccess}

import scala.collection.mutable.ListBuffer


sealed trait SemanticTree extends Ranged {

}

object SemanticTree{

  case class CodeGenError(msg: String) extends Exception

  def noTypeError[A](): A = throw CodeGenError("NoType encountered! (Type check failed)")

  def requireReturnType(semanticStatement: SemanticStatement, rqType: SType): Unit = {
    if(semanticStatement.returnType != rqType)
      throw SemanticError(s"Wrong return type, required return type: ${rqType.javaSimpleName}, " +
        s"found ${semanticStatement.returnType.javaSimpleName}, on statement $semanticStatement",
        semanticStatement)
  }

  def requireExprType(expr: SExpr, rqType: SType): Unit = {
    if(expr.exprType != rqType)
      throw SemanticError(s"Expression type mismatch, required type: ${rqType.javaSimpleName}, " +
        s"found ${expr.exprType.javaSimpleName}, on expression $expr",
        expr)
  }

  case class SPackage(classes: IndexedSeq[InternalType], typeContext: TypeContext) extends SemanticTree{
  }

  case class InternalType(pkg: String, simpleName: String, isAbstract: Boolean) extends SemanticTree with SRefType {

    override def javaSimpleName: String = pkg + "." + simpleName

    override def javaExtendedName: String = s"L$javaSimpleName;"

    override def arrayDimAndElemType: (Int, SType) = (0, this)

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
      s"class $javaSimpleName ${superClass.map("extends " + _).getOrElse("")}"
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

    def allMethods = localStaticMethods.values ++ localInstanceMethods.values

    def genClassFile(fileName: String, shouldLog: Boolean): ClassFile = {
      val cf = new ClassFile(fileName)
      localInstanceFields.values.foreach(_.genField(cf))
      localStaticFields.values.foreach(_.genField(cf))

      allMethods.foreach { _.genMethod(cf, shouldLog)}
      constructors.values.foreach(_.genConstructor(cf, shouldLog))

      cf
    }

  }

  case class InternalMethod(signature: MethodSignature, paramNames: IndexedSeq[TIdentifier], impl: Option[Block])
    extends SemanticTree {
    var sBlock: Option[SBlock] = None

    def genMethod(classFile: ClassFile, shouldLog: Boolean): Unit = {
      val mh = classFile.addMethod(signature.returns.jvmTypeName, signature.name, signature.args.map(_.jvmTypeName).toList)
      mh.setFlags(signature.flags)
      val ch = mh.codeHandler
      sBlock.foreach(b => b.genCode(ch))
      ch << BC.RETURN

      if(shouldLog) {
        println("***************")
        println(s"${if (signature.isStatic) "static" else "instance"} method ${signature.name} defined: ")
        ch.print
      }

      ch.freeze
    }
  }


  case class InternalField(signature: FieldSignature) extends SemanticTree {
    def genField(classFile: ClassFile): Unit = {
      val fh = classFile.addField(signature.fieldType.jvmTypeName, signature.name) //todo: add init code
      fh.setFlags(signature.flags)
    }
  }

  case class InternalConstructor(signature: ConstructorSignature, paramNames: IndexedSeq[TIdentifier], impl: Block) extends SemanticTree{
    var sBlock: SBlock = _

    def genConstructor(classFile: ClassFile, shouldLog: Boolean): Unit = {
      require(sBlock != null, "can't generate code on this constructor.")

      val mh = classFile.addConstructor(signature.args.map(_.jvmTypeName).toList)
      mh.setFlags(signature.flags)
      val ch = mh.codeHandler
      sBlock.genCode(ch)
      ch << BC.RETURN

      if(shouldLog) {
        println("***************")
        println(s"constructor (${signature.args.map(_.jvmTypeName).mkString(",")}) defined: ")
        ch.print
      }

      ch.freeze
    }
  }


  sealed trait SemanticStatement extends SemanticTree {
    def genCode(ch: CodeHandler): Unit
    def returnType: SType
  }

  case class ExprStatement(expr: SExpr) extends SemanticStatement{
    override def genCode(ch: CodeHandler): Unit = {
      expr.genCode(ch)
      if(expr.exprType.jvmTypeName != SVoid.jvmTypeName)
        ch << BC.POP
    }

    override def returnType: SType = SVoid
  }


  case class SBlock(parts: IndexedSeq[SemanticStatement], newDeclares: IndexedSeq[SLocalVar]) extends SemanticStatement {
    override val returnType = parts.foldRight(SVoid: SType){
      case (p,acc) => p.returnType + acc
    }


    override def toString: String = s"{[$returnType]" + parts.mkString(";") + "}"

    override def genCode(ch: CodeHandler): Unit = {
      newDeclares.foreach{v =>
        val i = ch.getFreshVar
        assert(i == v.index, "local index not match!")
      }

      parts.foreach(_.genCode(ch))
    }
  }

  case class SIf(condition: SExpr, thenBranch: SBlock, elseBranch: Option[SBlock]) extends SemanticStatement {
    requireExprType(condition, SBoolean)

    override val returnType = thenBranch.returnType + elseBranch.map(_.returnType).getOrElse(SVoid)

    override def genCode(ch: CodeHandler): Unit = {
      import cafebabe.AbstractByteCodes._

      val elseBranchLabel = ch.getFreshLabel("else_branch")
      val endLabel = ch.getFreshLabel("end_label")

      condition.genCode(ch)
      ch << IfEq(elseBranchLabel)
      thenBranch.genCode(ch)
      ch << AC.Goto(endLabel)
      ch << Label(elseBranchLabel)
      elseBranch.foreach(_.genCode(ch))
      ch << AC.Label(endLabel)
    }

    override def toString: String = s"if[$returnType]($condition) $thenBranch ${elseBranch.map("else" + _.toString).getOrElse("")}"
  }

  case class SWhile(condition: SExpr, body: SemanticStatement) extends SemanticStatement {
    requireExprType(condition, SBoolean)

    override val returnType = body.returnType

    override def genCode(ch: CodeHandler): Unit = {
      val outOfWhile = ch.getFreshLabel("out_of_while")
      val beforeWhile = ch.getFreshLabel("before_while")

      ch << AC.Label(beforeWhile)
      condition.genCode(ch)
      ch << AC.IfEq(outOfWhile)
      body.genCode(ch)
      ch << AC.Goto(beforeWhile)
      ch << AC.Label(outOfWhile)
    }

    override def toString: String = s"while($condition) $body"
  }

  case class SReturn(value: Option[SExpr]) extends SemanticStatement{

    override def genCode(ch: CodeHandler): Unit = {
      value.foreach(v => v.genCode(ch) )
      val ins = returnType match {
        case SVoid => BC.RETURN
        case s: SBasicType => s match {
          case SInt => BC.IRETURN
          case SChar => BC.IRETURN
          case SBoolean => BC.IRETURN
        }
        case r: SRefType => BC.ARETURN
        case SNoType => noTypeError()
      }
      ch << ins
    }

    override def returnType: SType = value.map(_.exprType).getOrElse(SVoid)

    override def toString: String = s"return[$returnType] ${value.getOrElse("")}"
  }

  case class SLocalVar(index: Int, name: String, exprType: SType) extends SExpr{
    override def genCode(ch: CodeHandler): Unit = {
      val bc = exprType match {
        case bs: SBasicType =>
          AC.ILoad(index) // todo: currently use iload to load all types of data
        case rf: SRefType =>
          AC.ALoad(index)
      }
      ch << bc
    }
  }

  case class BlockContext(typeContext: TypeContext, localMap: Map[String, SLocalVar], startIndex: Int,
                          thisType: SRefType, isStatic: Boolean){
    def isInstance = !isStatic

    val contextThis = if(isStatic) None else Some(SLocalVar(0, "this", thisType))

    def resolveType(jType: JType): SType = {
      typeContext.resolve(jType).getOrElse(throw SemanticError("Can't resolve type", jType))
    }

    /** resolve a single-word identifier, it could be:
      * <ul>
      *   <li> a local variable </li>
      *   <li> a static field in this class </li>
      *   <li> an instance field </li>
      * </ul>
      */
    def resolveIdentifierOpt(tIdentifier: TIdentifier): Option[SExpr] = {
      localMap.get(tIdentifier.data).orElse {
        thisType.getField(tIdentifier.data) match {
          case Some(f) if f.isStatic =>
            Some(StaticFieldAccess(f))
          case Some(f) if isInstance =>
            Some(SInstanceFieldAccess(contextThis.get, f))
          case _ => None
        }
      }
    }

    /** resolve a single-word identifier, it could be:
      * <ul>
      *   <li> a local variable </li>
      *   <li> a static field in this class </li>
      *   <li> an instance field </li>
      * </ul>
      */
    def resolveIdentifier(tIdentifier: TIdentifier): SExpr = {
      resolveIdentifierOpt(tIdentifier).getOrElse{
        throw SemanticError("Can't resolve identifier" ,tIdentifier)
      }
    }

    def longestMatchForAType(parts: Seq[TIdentifier]): (SRefType, Seq[TIdentifier]) = {
      val partial = ListBuffer[TIdentifier]()
      val left = parts.dropWhile(tk => {
        partial += tk
        typeContext.resolve(QualifiedIdent(partial.toIndexedSeq)).isEmpty
      }).drop(1)
      val sType = typeContext.resolve(QualifiedIdent(partial.toIndexedSeq)).getOrElse {
        throw SemanticError("Can't resolve qualified identifier", parts.head)
      }

      (sType, left)
    }

    /** resolve a chain of selection as a chain of instance field selection, </br>
      * for example:  localVar1.a.b </br>
      */
    def fieldSelectionChain(ins: SExpr, selections: Seq[TIdentifier]): SExpr = {
      var currentSelection = ins
      selections.foreach { t =>
        currentSelection.exprType match {
          case ref: SRefType =>
            val fSig = ref.getInstanceFiled(t.data).getOrElse{
              throw SemanticError(s"${t.data} is not an instance field of ${ref.javaSimpleName}", t)
            }
            currentSelection = SInstanceFieldAccess(currentSelection, fSig)
          case b => throw SemanticError(s"${t.data} is not an instance field of value type ${b.javaSimpleName}", t)
        }
      }
      currentSelection
    }

    /**
      * resolve a qualified identifier in the current block context
      * the result can be: <br>
      *   <ol>
      *   <li> a local variable or instance/static field </li>
      *   <li> a qualified type </li>
      *   <li> a static field on a qualified type </li>
      *   <li> an instance field on some static field on some qualified type </li>
      *   </ol>
      *
      *   [Some class] {static field selection} {instance field selection}
      * @throws SemanticError if failed to resolve
      */
    def resolveQualified(qualified: Seq[TIdentifier]): Either[SRefType,SExpr] = {
      val (selected: SExpr, leftToResolve) = resolveIdentifierOpt(qualified.head) match {
        case Some(p) => (p, qualified.tail) // it's already a field
        case None => // can only be static field selected on some type
          val (sType, left) = longestMatchForAType(qualified)
          if (left.isEmpty) return Left(sType) // no static field selection, just a type
          else {
            val staticSig = sType.getStaticFiled(left.head.data).getOrElse{
              throw SemanticError(s"${left.head.data} is not a static field of ${sType.javaSimpleName}", left.head)
            }
            (StaticFieldAccess(staticSig), left.tail) // static field selected
          }
      }

      Right(fieldSelectionChain(selected, leftToResolve))
    }

    def + (newVar: SLocalVar) = {
      copy(localMap = localMap+(newVar.name-> newVar), startIndex = startIndex + 1)
    }

    def createLocalVar(name: String, sType: SType, ranged: Ranged): SLocalVar = {
      SLocalVar(startIndex, name, sType) withRangeOf ranged
    }
  }

  sealed trait SExpr extends SemanticTree{
    def exprType: SType

    def genCode(ch: CodeHandler): Unit

  }

  object SExpr {

    case class StaticFieldAccess(f: FieldSignature) extends SExpr{
      require(f.isStatic)
      override def exprType: SType = f.fieldType

      override def toString: String = f.name

      override def genCode(ch: CodeHandler): Unit = {
        ch << AC.GetStatic(f.classType.jvmTypeName, f.name, f.fieldType.jvmTypeName)
      }
    }

    case class StaticFieldStore(f: FieldSignature, v: SExpr) extends SExpr{
      requireExprType(v, f.fieldType)
      require(f.isStatic)

      override def exprType: SType = f.fieldType

      override def genCode(ch: CodeHandler): Unit = {
        v.genCode(ch)

        ch << BC.DUP
        ch << AC.PutStatic(f.classType.jvmClassName, f.name, f.fieldType.jvmTypeName)
        ch << BC.SWAP
      }
    }


    case class SInstanceFieldAccess(ins: SExpr, f: FieldSignature) extends SExpr{
      require(! f.isStatic)
      override def exprType: SType = f.fieldType

      override def toString: String = f.name

      override def genCode(ch: CodeHandler): Unit = {
        ins.genCode(ch)

        if(ins.exprType.jvmTypeName.startsWith("[") && f.name == "length"){
          ch << BC.ARRAYLENGTH // So called "language hack" :)
        }else {
          ch << AC.GetField(f.classType.jvmTypeName, f.name, f.fieldType.jvmTypeName)
        }
      }
    }

    case class SInstanceFieldStore(ins: SExpr, f: FieldSignature, v: SExpr) extends SExpr{
      requireExprType(v, f.fieldType)
      require(! f.isStatic)

      override def exprType: SType = f.fieldType

      override def genCode(ch: CodeHandler): Unit = {
        ins.genCode(ch)
        v.genCode(ch)

        ch << BC.DUP
        ch << AC.PutField(f.classType.jvmClassName, f.name, f.fieldType.jvmTypeName)
        ch << BC.SWAP
      }
    }

    case class StaticMethodCall(f: MethodSignature, args: IndexedSeq[SExpr]) extends SExpr{
      require(f.isStatic)
      override def exprType: SType = f.returns

      override def toString: String = s"${f.name}(${args.mkString(",")})"

      override def genCode(ch: CodeHandler): Unit = {
        args.foreach(_.genCode(ch))
        ch << AC.InvokeStatic(f.classType.jvmClassName, f.name, f.signatureString)
      }
    }

    case class InstanceMethodCall(ins: SExpr, f: MethodSignature, args: IndexedSeq[SExpr]) extends SExpr{
      require(! f.isStatic)
      override def exprType: SType = f.returns

      override def genCode(ch: CodeHandler): Unit = {
        ins.genCode(ch)
        args.foreach(_.genCode(ch))
        ch << AC.InvokeVirtual(f.classType.jvmClassName, f.name, f.signatureString)
      }

      override def toString: String = s"$ins.${f.name}(${args.mkString(",")})"
    }

    case class SCreator(exprType: SType, args: IndexedSeq[SExpr]) extends SExpr{
      override def genCode(ch: CodeHandler): Unit = {
        ch << AC.New(exprType.jvmClassName)
        ch << BC.DUP

        args.foreach(_.genCode(ch))

        ch << AC.InvokeSpecial(exprType.jvmClassName, "<init>",
          ConstructorSignature.signatureString(args.map(_.exprType)))
      }
    }

    case class SConstructorCall(ins: SLocalVar ,constructorSignature: ConstructorSignature, args: IndexedSeq[SExpr]) extends SExpr{
      override def exprType: SType = constructorSignature.classType

      override def genCode(ch: CodeHandler): Unit = {
        ins.genCode(ch)
        ch << BC.DUP
        args.foreach(_.genCode(ch))
        ch << AC.InvokeSpecial(constructorSignature.classType.jvmClassName, "<init>",
          ConstructorSignature.signatureString(args.map(_.exprType)))
      }
    }

    case class SIntConst(data: Int) extends SExpr {
      override def exprType: SType = SInt

      override def genCode(ch: CodeHandler): Unit = ch << AC.Ldc(data)
    }

    case class SCharConst(data: Char) extends SExpr{
      override def exprType: SType = SChar

      override def genCode(ch: CodeHandler): Unit = ch << AC.Ldc(data)
    }

    case class SStringConst(data: String) extends SExpr{
      override def exprType: SType = ExternalType.string

      override def genCode(ch: CodeHandler): Unit = ch << AC.Ldc(data)
    }

    case class SBooleanConst(data: Boolean) extends SExpr{
      override def exprType: SType = SBoolean

      override def genCode(ch: CodeHandler): Unit = ch << AC.Ldc(if(data) 1 else 0)
    }

    case class SConvertString(expr: SExpr) extends SExpr {
      def failToConvert() = throw SemanticError(s"expression $expr can't be converted to String", expr)

      val wrapperType = expr.exprType match {
        case SInt => ExternalType.integer
        case SChar => ExternalType.char
        case SBoolean => ExternalType.boolean
        case _ => failToConvert()
      }

      override def exprType: SType = ExternalType.string

      override def genCode(ch: CodeHandler): Unit = {
        SCreator(wrapperType, IndexedSeq(expr)).genCode(ch)
        ch << AC.InvokeVirtual(wrapperType.jvmClassName, "toString", s"()${ExternalType.string.jvmTypeName}")
      }
    }

    object SConvertString{
      def convert(sExpr: SExpr): SExpr = {
        if(sExpr.exprType == ExternalType.string) sExpr
        else SConvertString(sExpr)
      }
    }

    trait SBinary extends SExpr{
      def l: SExpr
      def r: SExpr
      requireExprType(l, exprType)
      requireExprType(r, exprType)

      def genAfterArgs(ch: CodeHandler): Unit

      override def genCode(ch: CodeHandler): Unit = {
        l.genCode(ch)
        r.genCode(ch)
        genAfterArgs(ch)
      }
    }

    case class SStringConcat(l: SExpr, r: SExpr) extends SBinary{
      override def exprType: SType = ExternalType.string

      override def genAfterArgs(ch: CodeHandler): Unit = {
        val stringType = ExternalType.string.jvmTypeName
        ch << AC.InvokeVirtual(ExternalType.string.jvmClassName, "concat", s"($stringType)$stringType")
      }
    }

    case class SIAdd(l: SExpr, r: SExpr) extends SBinary{
      override def exprType: SType = SInt

      override def genAfterArgs(ch: CodeHandler): Unit = ch << BC.IADD
    }

    case class SISub(l: SExpr, r: SExpr) extends SBinary{
      override def exprType: SType = SInt

      override def genAfterArgs(ch: CodeHandler): Unit = ch << BC.ISUB
    }
    case class SIMul(l: SExpr, r: SExpr) extends SBinary{
      override def exprType: SType = SInt

      override def genAfterArgs(ch: CodeHandler): Unit = ch << BC.IMUL
    }
    case class SIDiv(l: SExpr, r: SExpr) extends SBinary{
      override def exprType: SType = SInt

      override def genAfterArgs(ch: CodeHandler): Unit = ch << BC.IDIV
    }

    case class SAssign(l: SExpr, r: SExpr) extends SExpr{
      requireExprType(r, l.exprType)

      override def exprType: SType = l.exprType

      override def genCode(ch: CodeHandler): Unit = {
        l match {
          case SLocalVar(idx, name, tp) =>
            val storeCode = tp match {
              case ref: SRefType => AC.AStore(idx)
              case _ => AC.IStore(idx) //todo currently use one statck depth for all primitive types
            }

            r.genCode(ch)
            ch << BC.DUP
            ch << storeCode
          case SArrayAccess(a,i,tp) =>
            SArrayStore(a, i, r).genCode(ch)
          case SInstanceFieldAccess(ins, f) =>
            SInstanceFieldStore(ins, f, r).genCode(ch)
          case StaticFieldAccess(f) =>
            StaticFieldStore(f, r).genCode(ch)
        }
      }
    }

    /**
      * Binary int comparison operation
      * @param abc the abstract byte code corresponding to this comparison operation,
      *            take a string jumping label as its argument
      */
    case class SBinaryIntComp(l: SExpr, r: SExpr, abc: String => AC.AbstractByteCode) extends SExpr {
      requireExprType(l, SInt)
      requireExprType(r, SInt)

      def exprType: SType = SBoolean

      def genCode(ch: CodeHandler): Unit = {
        l.genCode(ch)
        r.genCode(ch)

        val endLabel = ch.getFreshLabel("end_of_less_than")
        val lessLabel = ch.getFreshLabel("less_branch")

        ch << abc(lessLabel)
        ch << AC.Ldc(0)
        ch << AC.Goto(endLabel)

        ch << AC.Label(lessLabel)
        ch << AC.Ldc(1)

        ch << AC.Label(endLabel)
      }
    }

    sealed trait SUnary extends SExpr{
      def expr: SExpr
    }

    case class SUnaryNegate(expr: SExpr) extends SUnary{
      requireExprType(expr, SInt)
      override def exprType: SType = SInt

      override def genCode(ch: CodeHandler): Unit = {
        expr.genCode(ch)
        ch << AC.Ldc(-1)
        ch << BC.IMUL
      }
    }

    case class SUnaryNot(expr: SExpr) extends SUnary{
      requireExprType(expr, SBoolean)
      override def exprType: SType = SBoolean

      override def genCode(ch: CodeHandler): Unit = {
        val loadOne = ch.getFreshLabel("load_one")
        val endOfNot = ch.getFreshLabel("end_of_not")

        expr.genCode(ch)

        ch << AC.IfEq(loadOne)
        ch << AC.Ldc(0)
        ch << AC.Goto(endOfNot)

        ch << AC.Label(loadOne)
        ch << AC.Ldc(1)
        ch << AC.Label(endOfNot)
      }
    }

    case class SCast(expr: SExpr, toType: SType) extends SUnary {
      val (toDim, toElemType) = toType match {
        case ref: SRefType => ref.arrayDimAndElemType
        case _ => throw SemanticError("Can't cast to non-reference type", expr)
      }
      val needCode = expr.exprType match {
        case ref: SRefType =>
          val (fromDim, fromElemType) = ref.arrayDimAndElemType
          if(toType == ExternalType.obj) false
          else {
            if (fromDim != toDim)
              throw SemanticError("Can't cast to an array type with different dimension", expr)
            (fromElemType, toElemType) match {
              case (t1: SRefType, t2: SRefType) =>
                if (t1 isChildOf t2) false
                else if (t2 isChildOf t1) true
                else throw SemanticError(s"Cast $expr to unrelated type $toType", expr)
              case _ => throw SemanticError(s"Cast $expr to unrelated type $toType", expr)
            }
          }
      }

      override def exprType: SType = toType

      override def genCode(ch: CodeHandler): Unit = {
        expr.genCode(ch)
        if(needCode){
          ch << AC.CheckCast(toType.jvmClassName)
        }
      }
    }

    case class SArrayAccess(body: SExpr, idx: SExpr, exprType: SType) extends SExpr {
      requireExprType(idx, SInt)
      if(!body.exprType.isArrayType)
        throw SemanticError(s"$body is not an array", body)


      override def genCode(ch: CodeHandler): Unit = {
        body.genCode(ch)
        idx.genCode(ch)

        ch << BC.IALOAD
      }
    }

    case class SArrayStore(array: SExpr, idx: SExpr, value: SExpr) extends SExpr{
      requireExprType(idx, SInt)
      if(!array.exprType.isArrayType)
        throw SemanticError(s"$array is not an array", array)
      //todo require value type

      override def exprType: SType = value.exprType

      override def genCode(ch: CodeHandler): Unit = {
        array.genCode(ch)
        idx.genCode(ch)
        value.genCode(ch)

        ch << BC.DUP
        ch << BC.IASTORE
        ch << BC.SWAP
      }
    }

  }

}