package jmm_tests

import jmms.Tokenizer

/**
  * Created by weijiayi on 19/10/2016.
  */
class TokenizerTests extends MyTest {
  "tokenizer" should {
    "successfully parse all test source files" in {
      passSources.foreach(s =>{
        Tokenizer.tokenizeSource(s.mkString).isRight
      })

    }
  }
}
