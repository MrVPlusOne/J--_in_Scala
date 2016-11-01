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
      assert(string0.jvmTypeName === "Ljava/lang/String;", println(string0.jvmTypeName))
      assert(string0.javaExtendedName === "Ljava.lang.String;", println("lDotName: "+string0.javaExtendedName))
      assert(string0.javaSimpleName === "java.lang.String", println(string0.javaSimpleName))

      val string2 = TypeContext.default().resolve(RefTypeOrArray(SyntaxTree.Pen.qualified("java.lang.String"), 2)).get
      assert(string2.jvmTypeName === "[[Ljava/lang/String;", println(string2.jvmTypeName))

      val a0 = InternalType("main","A",isAbstract = false)
      assert(a0.jvmTypeName === "Lmain/A;", println(a0.jvmTypeName))
    }
  }

  "Code Generation" should {
    "compile all example files" in {
      passSources.foreach{
        case (src, srcName) =>
          val pkg = fullyAnalyze(src.mkString, srcName)
          pkg.classes.foreach { cl =>
            val cf = cl.genClassFile(cl.jvmClassName, shouldLog = true)
            cf.writeToFile(s"./compiled/${cl.jvmClassName}.class")
          }
      }
    }
  }


}
