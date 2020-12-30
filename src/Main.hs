module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import Data.Char (chr)
import Arch.Machine

main :: IO ()
main = do
    raw <- BL.readFile "data/challenge.bin"
    inputs <- readFile "data/solution.eventlog"
    let program = runGet listOfWord16 raw
    -- putStrLn $ unlines $ map show $ disassembleFrom 0x1652 program
    let result = converge step $ (load program) { input = inputs }
    loop result
    putStrLn $ message result

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
