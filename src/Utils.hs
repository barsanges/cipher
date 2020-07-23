{- |
   Module      : Utils
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Utility functions to format strings.
-}

module Utils (
  simplify,
  lighten
  ) where

import Data.Char ( isSpace, toUpper )

-- | Remove all spaces from the given string.
strip :: String -> String
strip = filter (not . isSpace)

-- | Convert some French non ASCII characters to ASCII characters (e.g.: à to a,
-- é to e, etc).
toASCII :: String -> String
toASCII [] = []
toASCII (c:cs) = go c ++ toASCII cs
  where
    go :: Char -> String
    go 'à' = "a"
    go 'â' = "a"
    go 'é' = "e"
    go 'è' = "e"
    go 'ê' = "e"
    go 'ë' = "e"
    go 'î' = "i"
    go 'ï' = "i"
    go 'ô' = "o"
    go 'ù' = "u"
    go 'û' = "u"
    go 'ü' = "u"
    go 'ç' = "c"
    go 'ÿ' = "y"
    go 'À' = "A"
    go 'Â' = "A"
    go 'Ç' = "C"
    go 'É' = "E"
    go 'È' = "E"
    go 'Ê' = "E"
    go 'Ë' = "E"
    go 'Î' = "I"
    go 'Ï' = "I"
    go 'Ô' = "O"
    go 'Ù' = "U"
    go 'Û' = "U"
    go 'Ü' = "U"
    go 'Ÿ' = "Y"
    go 'æ' = "ae" -- We can't use fmap because of these four!
    go 'Æ' = "AE"
    go 'œ' = "oe"
    go 'Œ' = "OE"
    go x = [x]

-- | Convert all letters to the corresponding upper-case letter, if any. Any
-- other character is left unchanged.
toUpperStr :: String -> String
toUpperStr = fmap toUpper

-- | Remove spaces and convert letters to uppercase ASCII letters if possible.
simplify :: String -> String
simplify = toUpperStr . toASCII . strip

-- | Split the list into sublists of length 'n'.
split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = (x0 : split n xs')
  where
    (x0, xs') = splitAt n xs

-- | Insert one white space every 'n' characters.
lighten :: Int -> String -> String
lighten n text = unwords $ split n text