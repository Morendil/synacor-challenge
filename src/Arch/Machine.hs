{-# LANGUAGE NamedFieldPuns #-}

module Arch.Machine where
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Word
import Data.Bits
import Data.Char
import Data.List
import Debug.Trace
import Text.Printf

data Opcode = Opcode {
    name :: String
  , width :: Word16
}

data Instruction = Instruction {
    op :: Opcode
  , args :: [Word16]
  , addr :: Word16
}

instance Show Instruction where
  show (Instruction { op = Opcode {name, width}, args, addr }) = (printf "0x%04x  " addr) ++ name ++ " [" ++ (intercalate "," $ map showVal args) ++ "]"
    where showVal val | val < 32768 = printf "0x%04x" val
          showVal reg = "r" ++ show (reg - 32768)

opcodes :: [(Word16, Opcode)]
opcodes = [
    (0,   Opcode { name = "halt",   width = 1} )
  , (1,   Opcode { name = "set",    width = 3} )
  , (2,   Opcode { name = "push",   width = 2} )
  , (3,   Opcode { name = "pop",    width = 2} )
  , (4,   Opcode { name = "eq",     width = 4} )
  , (5,   Opcode { name = "gt",     width = 4} )
  , (6,   Opcode { name = "jmp",    width = 2} )
  , (7,   Opcode { name = "jt",     width = 3} )
  , (8,   Opcode { name = "jf",     width = 3} )
  , (9,   Opcode { name = "add",    width = 4} )
  , (10,  Opcode { name = "mult",   width = 4} )
  , (11,  Opcode { name = "mod",    width = 4} )
  , (12,  Opcode { name = "and",    width = 4} )
  , (13,  Opcode { name = "or",     width = 4} )
  , (14,  Opcode { name = "not",    width = 3} )
  , (15,  Opcode { name = "rmem",   width = 3} )
  , (16,  Opcode { name = "wmem",   width = 3} )
  , (17,  Opcode { name = "call",   width = 2} )
  , (18,  Opcode { name = "ret",    width = 1} )
  , (19,  Opcode { name = "out",    width = 2} )
  , (20,  Opcode { name = "in",     width = 2} )
  , (21,  Opcode { name = "noop",   width = 1} )
  ]


disassemble :: [Word16] -> [Instruction]
disassemble program = disassembleFrom 0 program

disassembleFrom :: Word16 -> [Word16] -> [Instruction]
disassembleFrom offset program = disassemble' offset $ drop (fromInteger $ toInteger $ offset) program

disassemble' :: Word16 -> [Word16] -> [Instruction]
disassemble' _ [] = []
disassemble' offset words = instr:disassemble' (offset+width op) rest
  where instr = Instruction { op, args, addr }
        rest = drop opWidth words
        opWidth = fromInteger $ toInteger $ width op
        op = fromJust $ lookup (head words) opcodes
        args = tail $ take opWidth words
        addr = offset

data Machine = Machine {
      halted :: Bool
    , waiting :: Bool
    , pc :: Word16
    , memory :: M.Map Word16 Word16
    , registers :: M.Map Word16 Word16
    , stack :: [Word16]
    , output :: [Word16]
    , message :: String
    , input :: String
} deriving (Eq, Show)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

step :: Machine -> Machine
step m | halted m = m
step m | waiting m = m
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
  20 -> if null (input m) then m { waiting = True } else setreg ra (topIn m) $ popIn (m { pc = current + 2})
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

topIn :: Machine -> Word16
topIn m = (fromInteger . toInteger . ord) $ head (input m)

popIn :: Machine -> Machine
popIn m = m { input = tail $ input m }

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

feed :: String -> Machine -> Machine
feed line m = m { input = line, waiting = False }

noop :: Machine -> Machine
noop = id

initial :: Machine
initial = Machine {
        halted = False
      , waiting = False
      , pc = 0
      , memory = M.empty
      , registers = M.empty
      , output = []
      , stack = []
      , message = ""
      , input = ""
    }