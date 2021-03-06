package jmm_tests

import jmms.JToken._
import jmms.SyntaxTree._
import jmms.{JKeyword, SyntaxParser, Tokenizer}

/**
  * Created by weijiayi on 20/10/2016.
  */
class ParserTests extends MyTest{
  import jmms.SyntaxTree.JExpr._
  import jmms.SyntaxTree.Pen
  import jmms.JKeyword._


  val p = SyntaxParser

  def parseExpr(code: String) = p.parseString(p.pExpr, code)

  "expression parser" should {
    "parse basic elements" in {
      p.parseString(p.pIdent, "abc") shouldBe TIdentifier("abc")
      p.parseString(p.pLiteral, "5").literal shouldBe TInt(5)
      p.parseString(p.pOp("*"), "*") shouldBe TOp("*")
      p.parseString(p.pQualified, "A.bc.e") shouldBe Pen.qualified("A.bc.e")

      p.parseString(p.pType, "boolean[]") shouldBe BasicTypeArray(Pen.boolean_type, 1)
      p.parseString(p.pType, "a") shouldBe RefTypeOrArray(Pen.qualified("a"), 0)
      p.parseString(p.pType, "a.B.C[][]") shouldBe RefTypeOrArray(Pen.qualified("a.B.C"), 2)
      p.parseString(p.pType, "String") shouldBe RefTypeOrArray(Pen.qualified("String"), 0)


      p.parseString(p.pArguments, "(1+a, 'a')") shouldBe JArguments(
        Pen.plus(Pen.int(1), Pen.qualified("a")),
        JLiteral(TChar("a"))
      )

      p.parseString(p.pModifiers, "public static") shouldBe
        Modifiers(IndexedSeq(TReserve(JKeyword.k_public), TReserve(JKeyword.k_static)))


      // p.parseSource(p.pQualified, "") // expct to fail
    }

    "pass example1" in {
      parseExpr("52*32") shouldBe Pen.multiply(JLiteral(TInt(52)), JLiteral(TInt(32)))
      parseExpr("-i") shouldBe UnaryNegate(Pen.qualified("i"))
      parseExpr("(int) a") shouldBe BasicCast(Pen.int_type ,Pen.qualified("a"))
    }

    "pass example2" in {
      parseExpr("-a * (int) (b+A.c)") shouldBe Pen.multiply(
        UnaryNegate(Pen.qualified("a")),
        BasicCast(
          Pen.int_type,
          Pen.plus(Pen.qualified("b"), Pen.qualified("A.c"))
        )
      )
    }

    "parse Ref cast" in {
      parseExpr("(A.Data) variable") shouldBe RefTypeCast(
        RefTypeOrArray(Pen.qualified("A.Data"), 0),
        Pen.qualified("variable")
      )
    }

    "parse post expressions" in {
      parseExpr("a[1]") shouldBe PostExpr(Pen.qualified("a"), IndexedSeq(JArraySelector(Pen.int(1))))
      p.parseString(p.pExpr, "(a).m(1)") shouldBe PostExpr(
        Pen.qualified("a"),
        IndexedSeq(JQualifiedSelector(Pen.qualified("m"), Some(JArguments(Pen.int(1))))))
    }

    "parse local declarations" in {
      p.parseString(p.pVarDecl, "int a = 32;") shouldBe VarDecl(
        FormalParameter(Pen.int_type, TIdentifier("a")),
        Some(ExprInit(Pen.int(32)))
      )
      p.parseString(p.pVarDecl, "int a = {b,{}};") shouldBe VarDecl(
        FormalParameter(Pen.int_type, TIdentifier("a")),
        Some(ArrayInit(
          ExprInit(Pen.qualified("b")),
          ArrayInit()
        ))
      )
    }
  }

  "block parser" should{
    "parse if" in {
      p.parseString(p.pBlockStatement, """if(1<2) print("Ok!");""")
      p.parseString(p.pBlockStatement, """if(true) return a; else {{ }}""")
    }
    "parse while" in {
      p.parseString(p.pWhile,
        """while(i<5){
          | int a = i * 2;
          | a += "!";
          | print(a);
          |}
        """.stripMargin)
    }
  }

  "integrated parser" should{
    "parse an empty class" in {
      p.parseString(p.pCompilationUnit,
        """package a.empty;
          |
          |abstract class Empty{
          |  protected String a;
          |  private int test();
          |}
        """.stripMargin
      ) shouldBe CompilationUnit(
        Pen.qualified("a.empty"),
        IndexedSeq(),
        IndexedSeq(
          ClassDecl(
            Modifiers(IndexedSeq(TReserve(k_abstract))),
            TIdentifier("Empty"),
            None,
            body = IndexedSeq(
              Modifiers(IndexedSeq(TReserve(k_protected))) -> FieldMemberDecl(
                VarDecl(
                  FormalParameter(RefTypeOrArray(Pen.qualified("String"), 0), TIdentifier("a")),
                  initializer = None)
              ),
              Modifiers(IndexedSeq(TReserve(k_private))) -> MethodMemberDecl(
                Pen.int_type, TIdentifier("test"), IndexedSeq(), body = None
              )
            )
          )
        )
      )
    }

    "parse source file tests" in {
      passSources.foreach{ case (src, file) =>
        p.parseString(p.pCompilationUnit, src.mkString, file)
      }
    }
  }













}
