#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

import Data.Char (isNumber)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.ParserCombinators.ReadP
  ( ReadP,
    between,
    char,
    choice,
    munch1,
    option,
    pfail,
    readP_to_S,
    skipSpaces,
    string,
  )
import Text.Read (readMaybe)

data Tup3 a = Tup3 !a !a !a

deriving instance (Show a) => Show (Tup3 a)

data Ops = Dot deriving (Show)

tuple :: (Read a, Num a) => ReadP (Tup3 a)
tuple = between op cp $
  do
    a <- entry
    b <- comma *> entry
    c <- comma *> entry
    pure (Tup3 a b c)
  where
    comma = skipSpaces *> char ',' <* skipSpaces
    op = char '(' <* skipSpaces
    cp = skipSpaces *> char ')'

    entry = do
      x <- munch1 isNumber
      xs <- option "" (fmap ("." <>) $ char '.' *> munch1 isNumber)
      maybe pfail pure $ readMaybe (x <> xs)

ops :: ReadP Ops
ops =
  choice
    [ string "‧" >> pure Dot
    ]

parser :: ReadP (Tup3 Double, Ops, Tup3 Double)
parser =
  (,,)
    <$> (skipSpaces *> tuple <* skipSpaces)
    <*> (skipSpaces *> ops <* skipSpaces)
    <*> (skipSpaces *> tuple <* skipSpaces)

eval :: (Num a) => (Tup3 a, Ops, Tup3 a) -> a
eval (Tup3 a1 a2 a3, Dot, Tup3 b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

main :: IO ()
main =
  getArgs >>= \case
    [] -> failWithMessage "Missing expression"
    s : _ -> case readP_to_S parser s of
      (x, "") : _ -> print $ eval x
      _any -> failWithMessage $ "Invalid expression: " <> s
  where
    failWithMessage s = hPutStrLn stderr s >> exitFailure
