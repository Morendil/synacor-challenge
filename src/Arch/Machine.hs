module Arch.Machine where
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Bits

data Machine = Machine {
      halted :: Bool
    , pc :: Word16
    , memory :: M.Map Word16 Word16
    , registers :: M.Map Word16 Word16
    , stack :: [Word16]
    , output :: [Word16]
    , message :: String
} deriving (Eq, Show)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

step :: Machine -> Machine
step m | halted m = m
step m = case fetch m current of
  0 -> halt $ m { pc = current + 1}
  21 -> noop $ m { pc = current + 1}
  19 -> out a (m { pc = current + 2})
  6 -> jump a m
  7 -> jt a b (m { pc = current + 3})
  8 -> jf a b (m { pc = current + 3})
  1 -> setreg ra b (m { pc = current + 3})
  9 -> setreg ra ((b+c)`mod`32768) (m { pc = current + 4})
  4 -> setreg ra (if b==c then 1 else 0) (m { pc = current + 4})
  2 -> push a (m { pc = current + 2})
  3 -> setreg ra (top m) $ pop (m { pc = current + 2})
  5 -> setreg ra (if b>c then 1 else 0) (m { pc = current + 4})
  12 -> setreg ra (b .&. c) (m { pc = current + 4})
  13 -> setreg ra (b .|. c) (m { pc = current + 4})
  14 -> setreg ra (32767 `xor` b) (m { pc = current + 3})
  17 -> push (current + 2) (jump a m)
  10 -> setreg ra ((b*c)`mod`32768) (m { pc = current + 4})
  11 -> setreg ra (b `mod` c) (m { pc = current + 4})
  15 -> setreg ra (fetch m b) (m { pc = current + 3})
  16 -> write a b (m { pc = current + 3})
  18 -> pop $ jump (top m) m
  x -> crash ("Unexpected: " ++ show x ++ " at "++ show current) m
  where current = pc m
        a = fetch m $ current+1
        b = fetch m $ current+2
        c = fetch m $ current+3
        ra = raw m (current+1)

fetch :: Machine -> Word16 -> Word16
fetch m addr = if register then goRegister else plainValue
  where plainValue = fromMaybe 0 $ M.lookup addr (memory m)
        register = plainValue >= 32768
        goRegister = fromMaybe 0 $ M.lookup plainValue (registers m)

raw :: Machine -> Word16 -> Word16
raw m addr = fromMaybe 0 $ M.lookup addr (memory m)

reg :: Machine -> Word16 -> Word16
reg m addr = fromMaybe 0 $ M.lookup addr (registers m)

setreg :: Word16 -> Word16 -> Machine -> Machine
setreg which val m = m { registers = M.insert which val (registers m) }

load :: [Word16] -> Machine
load list =  initial { memory = M.fromList $ indexed list }
  where indexed = zip [0..]

halt :: Machine -> Machine
halt m = m { halted = True }

crash :: String -> Machine -> Machine
crash msg m = halt $ m { message = msg }

out :: Word16 -> Machine -> Machine
out val m = m { output = output m ++ [val] }

push :: Word16 -> Machine -> Machine
push val m = m { stack = val:(stack m) }

top :: Machine -> Word16
top m = head $ stack m

pop :: Machine -> Machine
pop m = m { stack = tail $ stack m }

jump :: Word16 -> Machine -> Machine
jump val m = m { pc = val }

jt :: Word16 -> Word16 -> Machine -> Machine
jt a b m = if a /= 0 then jump b m else m

jf :: Word16 -> Word16 -> Machine -> Machine
jf a b m = if a == 0 then jump b m else m

write :: Word16 -> Word16 -> Machine -> Machine
write addr val m = m { memory = M.insert addr val (memory m) }

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , pc = 0
      , memory = M.empty
      , registers = M.empty
      , output = []
      , stack = []
      , message = ""
    }