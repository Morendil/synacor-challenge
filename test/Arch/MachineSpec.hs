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
  describe "step" $ do
    it "should fetch and execute an instruction" $ do
      halted (step $ load [0]) `shouldBe` True
      pc (step $ load [0]) `shouldBe` 1
