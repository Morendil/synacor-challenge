module Arch.Machine where
import qualified Data.IntMap as I
import Data.Maybe (fromJust)

data Machine = Machine {
      halted :: Bool
    , pc :: Int
    , memory :: I.IntMap Int
    , output :: [Int]
}

step :: Machine -> Machine
step m = case fetch m current of
  Just 0 -> halt $ m { pc = current + 1}
  Just 21 -> noop $ m { pc = current + 1}
  Just 19 -> out a (m { pc = current + 2})
  Nothing -> m
  where current = pc m
        a = fromJust $ fetch m $ current+1

fetch :: Machine -> Int -> Maybe Int
fetch m addr = I.lookup addr (memory m)

load :: [Int] -> Machine
load list =  initial { memory = I.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

out :: Int -> Machine -> Machine
out val m = m { output = output m ++ [val] }

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = I.empty
      , output = []
    }