module Arch.Machine where

data Machine = Machine {
      halted :: Bool
    , pc :: Int
}

load :: [Int] -> Machine
load =  const initial

halt :: Machine -> Machine
halt m = m { halted = True }

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
    }