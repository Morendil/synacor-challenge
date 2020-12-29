module Arch.Machine where
import qualified Data.IntMap as I

data Machine = Machine {
      halted :: Bool
    , pc :: Int
    , memory :: I.IntMap Int
}

fetch :: Machine -> Int -> Maybe Int
fetch m addr = I.lookup addr (memory m)

load :: [Int] -> Machine
load list =  initial { memory = I.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = I.empty
    }