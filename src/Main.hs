module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.HashSet as H
import Data.Hashable
import qualified Data.Map as M
import Data.Binary.Get

import Data.Maybe
import Data.Char (chr)
import Arch.Machine
import Data.Function.Memoize

import Data.Graph.AStar

main :: IO ()
main = do
    raw <- BL.readFile "data/challenge.bin"
    inputs <- readFile "data/solution.eventlog"
    let program = runGet listOfWord16 raw
    -- putStrLn $ unlines $ map show $ filter (\(a,b) -> (b < 1000)) $ map (\n -> (n, mfn n 4 1)) [25734..]
    let result = converge step $ (load program) { input = inputs }
    loop result
    putStrLn $ message result

type Point = (Int, Int)
type Grid = M.Map Point Node
data Node = Num Int | Op (Int -> Int -> Int)
data State = State Point Int (Int -> Int)

instance Eq State where
    (==) (State c1 w1 _) (State c2 w2 _) = c1 == c2 && w1 == w2
instance Hashable State where
    hashWithSalt salt (State coord weight _) = hashWithSalt salt (coord, weight)
instance Ord State where
    compare (State c1 w1 _) (State c2 w2 _) = compare (c1, w1) (c2, w2)
instance Show State where
    show (State coord weight _) = show (coord, weight)

offsets :: [Point]
offsets = [(-1,0),(0,1),(1,0),(0,-1)]

add :: Point -> Point -> Point
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

neighbours :: Grid -> Point -> [(Point,Node)]
neighbours grid point = mapMaybe nearby offsets
    where nearby offset = let nearCoord = add point offset in ((,) nearCoord) <$> M.lookup nearCoord grid

grid :: Grid
grid = M.fromList [
      ((1,0),Op (+))
    , ((2,0),Num 4)
    , ((3,0),Op (*))
    , ((0,1),Op (-))
    , ((1,1),Num 4)
    , ((2,1),Op (*))
    , ((3,1),Num 8)
    , ((0,2),Num 9)
    , ((1,2),Op (-))
    , ((2,2),Num 11)
    , ((3,2),Op (-))
    , ((0,3),Op (*))
    , ((1,3),Num 18)
    , ((2,3),Op (*))
    , ((3,3),Num 1)
    ]

goal :: State -> Bool
goal (State coord weight _) = coord == (3,3) && weight == 30

next :: Grid -> State -> H.HashSet State
next grid (State coord weight fn) = H.fromList $ map nextState $ neighbours grid coord
    where nextState (point, (Op op)) = State point weight (op (fn weight))
          nextState (point, (Num n)) = State point (fn n) id

solution = aStar (next grid) (\a b -> 1) (\a -> 1) goal (State (0,0) 22 id)

mfn = memoize3 fn
fn :: Int -> Int -> Int -> Int
fn r7 0 r1 = (r1 + 1) `mod` 32768
fn r7 1 r1 = (r7 + r1 + 1) `mod` 32768
fn r7 2 r1 = ((2+r1) * r7 + r1 + 1) `mod` 32768
fn r7 r0 0 = (mfn r7 (r0-1) r7) `mod` 32768
fn r7 r0 r1 = (mfn r7 (r0-1) (mfn r7 r0 (r1-1))) `mod` 32768

loop :: Machine -> IO ()
loop m = do
    let result = converge step m
    putStrLn $ map (chr . fromInteger . toInteger) $ output $ result
    if waiting result then do {
        line <- getLine;
        loop $ feed (line++"\n") $ result { output = [] }
    } else return ()

listOfWord16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16le
             rest <- listOfWord16
             return (v : rest)
