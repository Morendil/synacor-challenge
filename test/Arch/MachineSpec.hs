module Arch.MachineSpec (spec) where

import Test.Hspec
import Arch.Machine

spec :: Spec
spec = do
  describe "halt" $ do
    it "should be halted after executing opCode 0, halt" $ do
      halted initial `shouldBe` False
      halted (halt initial) `shouldBe` True
