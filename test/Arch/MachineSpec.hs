module Arch.MachineSpec (spec) where

import Test.Hspec
import Arch.Machine
import Data.Char

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
  describe "rmem" $ do
    it "should set a register value from memory after opcode 15, rmem" $ do
      reg (step $ load [15, 32768, 0]) 32768 `shouldBe` 15
      reg (step $ step $ load [1, 32769, 2, 15, 32768, 32769]) 32768 `shouldBe` 2
  describe "wmem" $ do
    it "should set memory address a from value b after opcode 16, wmem" $ do
      fetch (step $ load [16, 0, 107]) 0 `shouldBe` 107
      fetch (step $ step $ load [1, 32768, 3, 16, 32768, 107]) 3 `shouldBe` 107
  describe "add" $ do
    it "should add b and c (mod 32768) arguments to a after opcode 9, add" $ do
      reg (step $ load [9, 32768, 30000, 30000]) 32768 `shouldBe` 27232
  describe "mult" $ do
    it "should multiply b and c (mod 32768) arguments to a after opcode 10, mul" $ do
      reg (step $ load [10, 32768, 30000, 30000]) 32768 `shouldBe` 26880
  describe "mod" $ do
    it "should write b mod c to a after opcode 11, mod" $ do
      reg (step $ load [11, 32768, 30000, 78]) 32768 `shouldBe` 48
  describe "eq" $ do
    it "should set a to comparison of b and c after opcode 4, eq" $ do
      reg (step $ load [4, 32768, 30000, 30000]) 32768 `shouldBe` 1
      reg (step $ load [4, 32769, 30000, 30001]) 32769 `shouldBe` 0
  describe "gt" $ do
    it "should set a to comparison of b and c after opcode 5, gt" $ do
      reg (step $ load [5, 32768, 30000, 30000]) 32768 `shouldBe` 0
      reg (step $ load [5, 32769, 30000, 30001]) 32769 `shouldBe` 0
      reg (step $ load [5, 32769, 30001, 30000]) 32769 `shouldBe` 1
  describe "and" $ do
    it "should set a to bitwise (b and c) after opcode 12, and" $ do
      reg (step $ load [12, 32768, 47, 21]) 32768 `shouldBe` 5
      reg (step $ load [12, 32768, 63, 21]) 32768 `shouldBe` 21
  describe "or" $ do
    it "should set a to bitwise (b or c) after opcode 13, or" $ do
      reg (step $ load [13, 32768, 47, 21]) 32768 `shouldBe` 63
  describe "not" $ do
    it "should set a to bitwise (not b) after opcode 14, not" $ do
      reg (step $ load [14, 32768, 23]) 32768 `shouldBe` 32744
  describe "push" $ do
    it "should push to the stack after opcode 2, push" $ do
      stack (step $ step $ load [2, 1, 2, 2]) `shouldBe` [2,1]
  describe "call" $ do
    it "should push to the stack and jump after opcode 17, call" $ do
      stack (step $ load [17, 100]) `shouldBe` [2]
      pc (step $ load [17, 100]) `shouldBe` 100
  describe "ret" $ do
    it "should pop and jump to the popped address after 18, ret" $ do
      stack (step $ step $ load [17, 3, 0, 18]) `shouldBe` []
      pc (step $ step $ load [17, 3, 0, 18]) `shouldBe` 2
  describe "pop" $ do
    it "should pop from the stack after opcode 3, pop" $ do
      stack (step $ step $ step $ load [2, 1, 2, 2, 3, 32768]) `shouldBe` [1]
      reg (step $ step $ step $ load [2, 1, 2, 2, 3, 32768]) 32768 `shouldBe` 2
  describe "input" $ do
    it "should mark the machine as pending if no input is available" $ do
      waiting (step $ load [20, 32768]) `shouldBe` True
    it "should pop from input after opcode 20, input" $ do
      reg (step $ feed "Foo" $ load [20, 32768]) 32768 `shouldBe` (fromInteger . toInteger . ord) 'F'
      input (step $ feed "Foo" $ load [20, 32768]) `shouldBe` "oo"
  describe "step" $ do
    it "should fetch and execute an instruction" $ do
      halted (step $ load [0]) `shouldBe` True
      pc (step $ load [0]) `shouldBe` 1
