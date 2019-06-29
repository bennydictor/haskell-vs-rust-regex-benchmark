module Main where

import Prelude hiding (seq)
import Lib
import System.Environment


main :: IO ()
main = do
    let a = sym (== 'a')
    let b = sym (== 'b')
    let re = rep (rep a `seq` b `seq` rep a `seq` b) `seq` rep a
    n <- read . (!! 0) <$> getArgs
    let m = match re $ concat (replicate n "ab")
    print m
