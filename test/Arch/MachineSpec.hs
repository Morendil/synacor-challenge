module Arch.MachineSpec (spec) where

import Test.Hspec
import Arch.Machine

spec :: Spec
spec = do
  describe "load" $ do
    it "should initialize a machine state with a given program" $ do
      pc (load [0]) `shouldBe` 0
      fetch (load [7]) 0 `shouldBe` Just 7
  describe "halt" $ do
    it "should be halted after executing opCode 0, halt" $ do
      halted initial `shouldBe` False
      halted (halt initial) `shouldBe` True
      step (halt initial) `shouldBe` halt initial
  describe "out" $ do
    it "should output one character after executing opcode 19, out" $ do
      output initial `shouldBe` []
      output (out 65 initial) `shouldBe` [65]
      output (step $ load [19, 65]) `shouldBe` [65]
      pc (step $ load [19, 65]) `shouldBe` 2
  describe "jmp" $ do
    it "should transfer pc after executing opcode 6, jmp" $ do
      pc initial `shouldBe` 0
      pc (jump 123 initial) `shouldBe` 123
      pc (step $ load [6, 123]) `shouldBe` 123
  describe "jmp" $ do
    it "should transfer pc after executing opcode 7, jt, if first arg is nonzero" $ do
      pc (jt 1 123 initial) `shouldBe` 123
      pc (step $ load [7, 1, 123]) `shouldBe` 123
    it "should leave pc alone after executing opcode 7, jt, if first arg is zero" $ do
      pc (step $ load [7, 0, 123]) `shouldBe` 3
  describe "step" $ do
    it "should fetch and execute an instruction" $ do
      halted (step $ load [0]) `shouldBe` True
      pc (step $ load [0]) `shouldBe` 1
