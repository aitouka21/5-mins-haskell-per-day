#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , free
             , recursion-schemes
-}

import Control.Comonad.Cofree
import Data.Functor.Foldable
import Debug.Trace
import GHC.Natural
import System.Environment

fib :: Natural -> Integer
fib = histo go
  where
    go :: Maybe (Cofree Maybe Integer) -> Integer
    go Nothing = traceStack "return 1 for 0" 1
    go (Just (_ :< Nothing)) = traceStack "return 1 for 1" 1
    go (Just (a :< Just (b :< _))) = traceStack ("return " ++ show a ++ " + " ++ show b) (a + b)

fib' :: Natural -> Integer
fib' 0 = traceStack "return 1 for 0" 1
fib' 1 = traceStack "return 1 for 1" 1
fib' n = traceStack ("return " ++ show a ++ " + " ++ show b) (a + b)
  where
    a = fib' (n - 1)
    b = fib' (n - 2)

main :: IO ()
main = do
  fn : n : _ <- getArgs
  print $
    fmap
      ( case fn of
          "fib1" -> fib
          "fib2" -> fib'
      )
      [0 .. (read n)]
