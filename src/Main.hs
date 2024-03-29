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
   str <- case input cli of
            Message m -> pure m
            InputFile f -> readFile f
            StdIn -> getContents
   let str' = simplify str
   let new = if decrypt cli
             then decode (cipher cli) str'
             else lighten blockSize $ encode (cipher cli) str'
   case output cli of
      OutputFile f -> writeFile f new
      StdOut -> putStrLn new

-- | Size of the blocks of characters in the output.
blockSize :: Int
blockSize = 4

-- | Encode the text with the given cipher.
encode :: Cipher -> String -> String
encode (Caesar shift) = caesar shift
encode (Vigenere key) = vigenere (simplify key)
encode Polybius = polybius
encode (Zigzag n) = zigzag n

-- | Decode the text with the given cipher.
decode :: Cipher -> String -> String
decode (Caesar shift) = caesar' shift
decode (Vigenere key) = vigenere' (simplify key)
decode Polybius = polybius'
decode (Zigzag n) = zigzag' n
