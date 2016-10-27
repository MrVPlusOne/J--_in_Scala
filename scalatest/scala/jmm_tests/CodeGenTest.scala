package jmm_tests

import cafebabe.ClassFile
import jmms.SemanticTree.InternalType
import jmms.SyntaxTree.RefTypeOrArray
import jmms.{SyntaxTree, TypeContext}

import scala.io.Source

/**
  * Created by weijiayi on 26/10/2016.
  */
class CodeGenTest extends MyTest{

  "code name" should {
    "be well formed" in {
      val string0 = TypeContext.default().resolve(RefTypeOrArray(SyntaxTree.Pen.qualified("java.lang.String"), 0)).get
      assert(string0.genTypeName === "Ljava/lang/String;", println(string0.genTypeName))
      assert(string0.lDotName === "Ljava.lang.String;", println("lDotName: "+string0.lDotName))
      assert(string0.queryName === "java.lang.String", println(string0.queryName))

      val string2 = TypeContext.default().resolve(RefTypeOrArray(SyntaxTree.Pen.qualified("java.lang.String"), 2)).get
      assert(string2.genTypeName === "[[Ljava/lang/String;", println(string2.genTypeName))

      val a0 = InternalType("main","A",isAbstract = false)
      assert(a0.genTypeName === "Lmain/A;", println(a0.genTypeName))
    }
  }

}
