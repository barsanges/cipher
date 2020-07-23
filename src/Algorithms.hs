{- |
   Module      : Algorithms
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Simple algorithms to encrypt / decrypt strings.
-}

module Algorithms (
  caesar,
  caesar',
  vigenere,
  vigenere',
  polybius,
  polybius'
  ) where

import Prelude hiding ( lookup, map )
import Data.Bimap ( Bimap, insert, empty, size, lookup, (!), lookupR, map )
import Data.Maybe ( fromMaybe )

-- | ASCII alphabet.
ascii :: Bimap Int Char
ascii = foldl (flip =<< insert . size) empty ['A'..'Z']

-- | Number of characters in the alphabet (hopefully 26!).
asciiSize :: Int
asciiSize = size ascii

-- | Replaces each ASCII capital letter in the string by a letter a number
-- of positions down the alphabet.
caesar :: Int -> String -> String
caesar shift = fmap go
  where
    go x = case lookupR x ascii of
      Nothing -> x
      Just i -> ascii ! ((i + shift) `mod` asciiSize)

-- | Decrypt a string encrypted with 'caesar shift'.
caesar' :: Int -> String -> String
caesar' shift = caesar (-shift)

-- | Replaces each ASCII capital letter in the string (second argument) by a
-- letter a number of positions down the alphabet depending on the key (first
-- argument).
vigenere :: String -> String -> String
vigenere [] str = str
vigenere key str = zipWith go (cycle key) str
  where
    go k x = fromMaybe x (char k x)
    char k x = do
      k' <- lookupR k ascii
      x' <- lookupR x ascii
      let y' = (x' + k' + 1) `mod` asciiSize
      lookup y' ascii

-- | Decrypt a string encrypted with 'vigenere key'.
vigenere' :: String -> String -> String
vigenere' [] str = str
vigenere' key str = zipWith go (cycle key) str
  where
    go k x = fromMaybe x (char' k x)
    char' k x = do
      k' <- lookupR k ascii
      x' <- lookupR x ascii
      let y' = (x' - k' - 1) `mod` asciiSize
      lookup y' ascii

-- | Polybius square.
square :: Bimap String Char
square = map go ascii
  where
    len = ceiling . sqrt . fromIntegral $ asciiSize
    go x = (show $ x `div` len) ++ (show $ x `mod` len)

-- | Replaces each ASCII capital letter in the string by a two digit number.
polybius :: String -> String
polybius [] = []
polybius (x:xs) = x' ++ polybius xs
  where
    x' = fromMaybe [x] (lookupR x square)

-- | Decrypt a string encrypted with 'polybius'. Each pair of digits is treated
-- as an ASCII character.
polybius' :: String -> String
polybius' [] = []
polybius' [x] = [x]
polybius' (x1:x2:xs) = x' ++ polybius' xs
  where
    x' = case lookup [x1, x2] square of
      Just c -> [c]
      Nothing -> [x1, x2]