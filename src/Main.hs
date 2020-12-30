module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import Data.Char (chr)
import Arch.Machine
import Data.Function.Memoize

main :: IO ()
main = do
    raw <- BL.readFile "data/challenge.bin"
    inputs <- readFile "data/solution.eventlog"
    let program = runGet listOfWord16 raw
    -- putStrLn $ unlines $ map show $ filter (\(a,b) -> (b < 1000)) $ map (\n -> (n, mfn n 4 1)) [25734..]
    let result = converge step $ (load program) { input = inputs }
    loop result
    putStrLn $ message result

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
        let cheat = setreg 32775 (read line) result
            normal = feed (line++"\n") $ result { output = [] }
        in if head line == '0' then loop cheat else loop normal
    } else return ()

listOfWord16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16le
             rest <- listOfWord16
             return (v : rest)
