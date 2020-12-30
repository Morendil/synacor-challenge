module Arch.Machine where
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Word

data Machine = Machine {
      halted :: Bool
    , pc :: Word16
    , memory :: M.Map Word16 Word16
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
  Just 6 -> jump a m
  Just 7 -> jt a b (m { pc = current + 3})
  Just 8 -> jf a b (m { pc = current + 3})
  Just x -> crash ("Unexpected: " ++ show x ++ " at "++ show current) m
  Nothing -> m
  where current = pc m
        a = fromJust $ fetch m $ current+1
        b = fromJust $ fetch m $ current+2

fetch :: Machine -> Word16 -> Maybe Word16
fetch m addr = M.lookup addr (memory m)

load :: [Word16] -> Machine
load list =  initial { memory = M.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

crash :: String -> Machine -> Machine
crash msg m = halt $ m { message = msg }

out :: Word16 -> Machine -> Machine
out val m = m { output = output m ++ [val] }

jump :: Word16 -> Machine -> Machine
jump val m = m { pc = val }

jt :: Word16 -> Word16 -> Machine -> Machine
jt a b m = if a /= 0 then jump b m else m

jf :: Word16 -> Word16 -> Machine -> Machine
jf a b m = if a == 0 then jump b m else m

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = M.empty
      , output = []
      , message = ""
    }