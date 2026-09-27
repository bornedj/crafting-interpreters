module Main (main) where

import Lexer
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Lexer.fewCharToken" $ do
    context "double character sequences" $ do
      it "returns double equal" $ do
        let doubleEquals = Input 0 "=="
            expected = Right (Input 2 "", FewChar EqualEqual)
            result = runParser fewCharToken doubleEquals
        result `shouldBe` expected
      it "returns not equal" $ do
        let notEqual = Input 0 "!="
            expected = Right (Input 2 "", FewChar BangEqual)
            result = runParser fewCharToken notEqual
        result `shouldBe` expected
      it "returns greater equal" $ do
        let greaterEqual = Input 0 ">="
            expected = Right (Input 2 "", FewChar GreaterEqual)
            result = runParser fewCharToken greaterEqual
        result `shouldBe` expected
      it "returns lesser equal" $ do
        let lesserEqual = Input 0 "<="
            expected = Right (Input 2 "", FewChar LesserEqual)
            result = runParser fewCharToken lesserEqual
        result `shouldBe` expected
  describe "Lexer.keywordToken" $ do
    it "returns and" $ do
      let and' = Input 0 "and"
          expected = Right (Input 3 "", Keyword And)
          result = runParser keywordToken and'
      result `shouldBe` expected
    it "returns class" $ do
      let class' = Input 0 "class"
          expected = Right (Input 5 "", Keyword Class)
          result = runParser keywordToken class'
      result `shouldBe` expected
    it "returns else" $ do
      let else' = Input 0 "else"
          expected = Right (Input 4 "", Keyword Else)
          result = runParser keywordToken else'
      result `shouldBe` expected
    it "returns fun" $ do
      let fun = Input 0 "fun"
          expected = Right (Input 3 "", Keyword Fun)
          result = runParser keywordToken fun
      result `shouldBe` expected
    it "returns for" $ do
      let for' = Input 0 "for"
          expected = Right (Input 3 "", Keyword For)
          result = runParser keywordToken for'
      result `shouldBe` expected
    it "returns if" $ do
      let if' = Input 0 "if"
          expected = Right (Input 2 "", Keyword If)
          result = runParser keywordToken if'
      result `shouldBe` expected
    it "returns nil" $ do
      let nil = Input 0 "nil"
          expected = Right (Input 3 "", Keyword Nil)
          result = runParser keywordToken nil
      result `shouldBe` expected
    it "returns or" $ do
      let or' = Input 0 "or"
          expected = Right (Input 2 "", Keyword Or)
          result = runParser keywordToken or'
      result `shouldBe` expected
    it "returns print" $ do
      let print' = Input 0 "print"
          expected = Right (Input 5 "", Keyword Print)
          result = runParser keywordToken print'
      result `shouldBe` expected
    it "returns return" $ do
      let return' = Input 0 "return"
          expected = Right (Input 6 "", Keyword Return)
          result = runParser keywordToken return'
      result `shouldBe` expected
    it "returns super" $ do
      let super = Input 0 "super"
          expected = Right (Input 5 "", Keyword Super)
          result = runParser keywordToken super
      result `shouldBe` expected
    it "returns this" $ do
      let this = Input 0 "this"
          expected = Right (Input 4 "", Keyword This)
          result = runParser keywordToken this
      result `shouldBe` expected
    it "returns var" $ do
      let var = Input 0 "var"
          expected = Right (Input 3 "", Keyword Var)
          result = runParser keywordToken var
      result `shouldBe` expected
    it "returns while" $ do
      let while = Input 0 "while"
          expected = Right (Input 5 "", Keyword While)
          result = runParser keywordToken while
      result `shouldBe` expected
