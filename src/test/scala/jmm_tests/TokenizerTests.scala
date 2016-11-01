package jmm_tests

import jmms.JToken.TString
import jmms.Tokenizer

import scala.util.parsing.input.NoPosition

/**
  * Created by weijiayi on 19/10/2016.
  */
class TokenizerTests extends MyTest {
  "JTokens" should {
    "equal regardless of position" in {
      TString("hi").withRange(5,6,NoPosition) shouldBe TString("hi")
      assert( TString("hi") !== TString("yep") )
    }
  }

  "tokenizer" should {
    "successfully parse all test source files" in {
      passSources.foreach{case (s, f) =>
        Tokenizer.tokenizeSource(s.mkString).isRight
      }
    }
  }
}
