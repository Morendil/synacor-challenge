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
    putStrLn $ map (chr . fromInteger . toInteger) $ output $ result
    putStrLn $ message result

listOfWord16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16le
             rest <- listOfWord16
             return (v : rest)
