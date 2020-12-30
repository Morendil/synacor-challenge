module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import Data.Char (chr)
import Arch.Machine

main :: IO ()
main = do
    raw <- BL.readFile "data/challenge.bin"
    let program = runGet listOfWord16 raw
    let result = converge step $ load program
    loop result
    putStrLn $ message result

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
