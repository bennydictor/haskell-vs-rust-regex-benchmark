{-# LANGUAGE NumericUnderscores #-}
module Main where

import Prelude hiding (seq)
import Lib
import Criterion.Main
import Criterion.Types


main :: IO ()
main = do
    let a = sym (== 'a')
    let b = sym (== 'b')
    let re = rep (rep a `seq` b `seq` rep a `seq` b) `seq` rep a
    let benchTarget s n = match re $ concat $ replicate n s
    defaultMainWith (defaultConfig { timeLimit = 60 })
        [ bgroup "ab"
            [ bench   "1_000" $ nf (benchTarget "ab")   1_000
            , bench  "10_000" $ nf (benchTarget "ab")  10_000
            , bench "100_000" $ nf (benchTarget "ab") 100_000 ] ]
