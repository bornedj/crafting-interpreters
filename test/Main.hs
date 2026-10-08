module Main (main) where

import Lexer
import Test.Hspec
import Text.Printf (printf)

main :: IO ()
main = hspec $ do
  describe "Lexer.fewCharToken" $ do
    context "double character sequences" $ do
      it "returns double equal" $ do
        let doubleEquals = mkInput "=="
            expected = Right (Input 1 2 "", FewChar EqualEqual)
            result = runParser fewCharToken doubleEquals
        result `shouldBe` expected
      it "returns not equal" $ do
        let notEqual = mkInput "!="
            expected = Right (Input 1 2 "", FewChar BangEqual)
            result = runParser fewCharToken notEqual
        result `shouldBe` expected
      it "returns greater equal" $ do
        let greaterEqual = mkInput ">="
            expected = Right (Input 1 2 "", FewChar GreaterEqual)
            result = runParser fewCharToken greaterEqual
        result `shouldBe` expected
      it "returns lesser equal" $ do
        let lesserEqual = mkInput "<="
            expected = Right (Input 1 2 "", FewChar LesserEqual)
            result = runParser fewCharToken lesserEqual
        result `shouldBe` expected
  describe "Lexer.singleCharToken" $ do
    let expectedInput = Input 1 1 ""
        testCases =
          [ (mkInput "(", "left paren", Right (expectedInput, SingleChar LeftParen)),
            (mkInput ")", "right paren", Right (expectedInput, SingleChar RightParen)),
            (mkInput "{", "left brace", Right (expectedInput, SingleChar LeftBrace)),
            (mkInput "}", "right brace", Right (expectedInput, SingleChar RightBrace)),
            (mkInput ",", "comma", Right (expectedInput, SingleChar Comma)),
            (mkInput ".", "dot", Right (expectedInput, SingleChar Dot)),
            (mkInput "-", "minus", Right (expectedInput, SingleChar Minus)),
            (mkInput "+", "plus", Right (expectedInput, SingleChar Plus)),
            (mkInput ";", "semicolon", Right (expectedInput, SingleChar Semicolon)),
            (mkInput "/", "slash", Right (expectedInput, SingleChar Slash)),
            (mkInput "*", "star", Right (expectedInput, SingleChar Star))
          ]
    mapM_
      ( \(input, description, expected) -> do
          it (printf "returns %s" description) $ do
            runParser singleCharToken input `shouldBe` expected
      )
      testCases
