module Main (main) where

import Test.Hspec
import Parser

main :: IO ()
main = hspec $ do
  describe "cleanLine" $ do
    it "removes comments" $ do
      cleanLine "D=A // set D to A" `shouldBe` "D=A"

    it "removes all whitespace" $ do
      cleanLine "  @ 1 2 3  " `shouldBe` "@123"

    it "handles comment-only lines" $ do
      cleanLine "// comment only" `shouldBe` ""

  describe "parseInstruction - A-instruction" $ do
    it "parses simple A-instruction" $ do
      parseInstruction "@123" `shouldBe` Just (AInstruction 123)

    it "parses zero" $ do
      parseInstruction "@0" `shouldBe` Just (AInstruction 0)

    -- TODO(human): Add test for invalid A-instruction like "@abc"

  describe "parseInstruction - L-instruction" $ do
    it "parses label" $ do
      parseInstruction "(LOOP)" `shouldBe` Just (LInstruction "LOOP")

    -- TODO(human): Add test for another label like "(END)"

  describe "parseInstruction - C-instruction" $ do
    it "parses dest=comp" $ do
      parseInstruction "D=M" `shouldBe` Just (CInstruction (Just "D") "M" Nothing)

    it "parses comp;jump" $ do
      parseInstruction "D;JGT" `shouldBe` Just (CInstruction Nothing "D" (Just "JGT"))

    it "parses dest=comp;jump" $ do
      parseInstruction "D=D+1;JMP" `shouldBe` Just (CInstruction (Just "D") "D+1" (Just "JMP"))

    -- TODO(human): Add test for comp-only instruction like "D+1"

  describe "parseLines" $ do
    it "parses multiple lines" $ do
      let input = "@2\nD=A\n// comment\n@3"
      parseLines input `shouldBe` [AInstruction 2, CInstruction (Just "D") "A" Nothing, AInstruction 3]

    -- TODO(human): Add test with labels and empty lines
