module Arch.Machine where
import qualified Data.IntMap as I
import Data.Maybe (fromJust)
import Data.Word

data Machine = Machine {
      halted :: Bool
    , pc :: Int
    , memory :: I.IntMap Word16
    , output :: [Word16]
    , message :: String
} deriving (Eq, Show)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

step :: Machine -> Machine
step m | halted m = m
step m = case fetch m current of
  Just 0 -> halt $ m { pc = current + 1}
  Just 21 -> noop $ m { pc = current + 1}
  Just 19 -> out a (m { pc = current + 2})
  Just x -> crash ("Unexpected: " ++ show x ++ " at "++ show current) m
  Nothing -> m
  where current = pc m
        a = fromJust $ fetch m $ current+1

fetch :: Machine -> Int -> Maybe Word16
fetch m addr = I.lookup addr (memory m)

load :: [Word16] -> Machine
load list =  initial { memory = I.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

crash :: String -> Machine -> Machine
crash msg m = halt $ m { message = msg }

out :: Word16 -> Machine -> Machine
out val m = m { output = output m ++ [val] }

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = I.empty
      , output = []
      , message = ""
    }