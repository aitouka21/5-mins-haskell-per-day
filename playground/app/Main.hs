module Main (main) where

import Control.Lens
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

{-
problem: https://toj.tfcis.org/oj/pro/691/

given that: 1 <= n <= 100 1, <= m <= 200000
we don't need to compute the whole sequence, just up to m is ok

n =  1: S
n =  2: B
n =  3: SB
n =  4: BSB
n =  5: SBBSB
n =  6: BSBSBBSB
n =  7: SBBSBBSBSBBSB
n =  8: BSBSBBSBSBBSBBSBSBBSB
n =  9: SBBSBBSBSBBSBBSBSBBSBSBBSBBSBSBBSB
n = 10: BSBSBBSBSBBSBBSBSBBSBSBBSBBSBSBBSBBSBSBBSBSBBSBBSBSBBSB

example:
>>> adapter $ cellOf 5 1
0

>>> adapter $ cellOf 5 5
1

>>> adapter $ cellOf 5 6
-1

>>> adapter $cellOf 100 200002
1
-}

main :: IO ()
main =
  getArgs >>= f . map readMaybe . take 2
  where
    f ((Just n) : (Just m) : _) = print $ adapter $ cellOf n m
    f _ = hPutStrLn stderr "invalid input" >> exitFailure

data Cell = S | B deriving (Show)

adapter :: Maybe Cell -> Int
adapter Nothing = -1
adapter (Just S) = 0
adapter (Just B) = 1

cellOf :: Int -> Int -> Maybe Cell
cellOf n m = iterate run [S] !! (n - 1) ^? element (m - 1)

run :: [Cell] -> [Cell]
run [] = []
run (S : xs) = B : run xs
run (B : xs) = S : B : run xs
