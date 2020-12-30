module Arch.MachineSpec (spec) where

import Test.Hspec
import Arch.Machine

spec :: Spec
spec = do
  describe "load" $ do
    it "should initialize a machine state with a given program" $ do
      pc (load [0]) `shouldBe` 0
      fetch (load [7]) 0 `shouldBe` 7
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
  describe "jt" $ do
    it "should transfer pc after executing opcode 7, jt, if first arg is nonzero" $ do
      pc (jt 1 123 initial) `shouldBe` 123
      pc (step $ load [7, 1, 123]) `shouldBe` 123
    it "should leave pc alone after executing opcode 7, jt, if first arg is zero" $ do
      pc (step $ load [7, 0, 123]) `shouldBe` 3
  describe "jf" $ do
    it "should transfer pc after executing opcode 8, jf, if first arg is zero" $ do
      pc (jf 0 123 initial) `shouldBe` 123
      pc (step $ load [8, 0, 123]) `shouldBe` 123
    it "should leave pc alone after executing opcode 8, jf, if first arg is nonzero" $ do
      pc (step $ load [8, 1, 123]) `shouldBe` 3
  describe "register mode" $ do
    it "should interpret arguments in the range 32768-32775 as register references" $ do
      pc (step $ load [7, 1, 32768]) `shouldBe` 0
  describe "set" $ do
    it "should set a register value after executing opcode 1, set" $ do
      reg (step $ load [1, 32768, 107]) 32768 `shouldBe` 107
  describe "add" $ do
    it "should add b and c (mod 32768) arguments to a after opcode 9, add" $ do
      reg (step $ load [9, 32768, 30000, 30000]) 32768 `shouldBe` 27232
  describe "eq" $ do
    it "should set a to comparison of b and c after opcode 4, eq" $ do
      reg (step $ load [4, 32768, 30000, 30000]) 32768 `shouldBe` 1
      reg (step $ load [4, 32769, 30000, 30001]) 32769 `shouldBe` 0
  describe "push" $ do
    it "should push to the stack after opcode 2, push" $ do
      stack (step $ step $ load [2, 1, 2, 2]) `shouldBe` [2,1]
  describe "pop" $ do
    it "should pop from the stack after opcode 3, pop" $ do
      stack (step $ step $ step $ load [2, 1, 2, 2, 3, 32768]) `shouldBe` [1]
      reg (step $ step $ step $ load [2, 1, 2, 2, 3, 32768]) 32768 `shouldBe` 2
  describe "step" $ do
    it "should fetch and execute an instruction" $ do
      halted (step $ load [0]) `shouldBe` True
      pc (step $ load [0]) `shouldBe` 1
