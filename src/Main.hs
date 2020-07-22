{- |
   Module      : Main
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Parses command-line options and calls the appropriate cipher.
-}

module Main where

import CLI
import Algorithms
import Utils

-- | Application entry point.
main :: IO ()
main = do
   cli <- execParser args
   str <- case source cli of
            Message m -> pure m
            File f -> readFile f
   let str' = simplify str
   let new = if decrypt cli
             then decode (cipher cli) str'
             else encode (cipher cli) str'
   putStrLn (lighten blockSize new) -- TODO: allow to write to a file.

-- | Size of the blocks of characters in the output.
blockSize :: Int
blockSize = 4

-- | Encode the text with the given cipher.
encode :: Cipher -> String -> String
encode (Caesar shift) = caesar shift
encode (Vigenere key) = vigenere (simplify key)
encode Polybius = polybius

-- | Decode the text with the given cipher.
decode :: Cipher -> String -> String
decode (Caesar shift) = caesar' shift
decode (Vigenere key) = vigenere' (simplify key)
decode Polybius = polybius'