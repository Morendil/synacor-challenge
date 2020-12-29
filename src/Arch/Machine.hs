module Arch.Machine where
import qualified Data.IntMap as I

data Machine = Machine {
      halted :: Bool
    , pc :: Int
    , memory :: I.IntMap Int
}

step :: Machine -> Machine
step m = case fetch m (pc m) of
  Just 0 -> halt $ m { pc = pc m + 1}
  Just 21 -> noop $ m { pc = pc m + 1}
  Nothing -> m

fetch :: Machine -> Int -> Maybe Int
fetch m addr = I.lookup addr (memory m)

load :: [Int] -> Machine
load list =  initial { memory = I.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = I.empty
    }