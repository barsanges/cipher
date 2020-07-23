{- |
   Module      : CLI
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Command line interface for 'cipher'.
-}

module CLI (
  Cipher(..),
  Input(..),
  Args(..),
  args,
  execParser -- Re-exported from Options.Applicative
  ) where

import Options.Applicative

data Cipher = Caesar Int
            | Vigenere String
            | Polybius

data Input = Message String
            | File String
            | StdIn

-- FIXME: allow to write to file.

data Args = Args
  { cipher :: Cipher
  , input :: Input
  , decrypt :: Bool
  }

caesarParser :: Parser Cipher
caesarParser = Caesar <$> option auto
  ( long "caesar"
  <> short 'c'
  <> metavar "SHIFT"
  <> help "Use Caesar cipher with a given (integer) shift"
  )

vigenereParser :: Parser Cipher
vigenereParser = Vigenere <$> strOption
  ( long "vigenere"
  <> short 'v'
  <> metavar "KEY"
  <> help "Use Vigenère cipher with a given key"
  )

polybiusParser :: Parser Cipher
polybiusParser = flag' Polybius
  ( long "polybius"
  <> short 'p'
  <> help "Use Polybius cipher")

cipherParser :: Parser Cipher
cipherParser = caesarParser
             <|> vigenereParser
             <|> polybiusParser

messageParser :: Parser Input
messageParser = Message <$> strOption
  ( long "message"
  <> short 'm'
  <> metavar "MESSAGE"
  <> help "Encrypt 'MESSAGE'"
  )

fileParser :: Parser Input
fileParser = File <$> strOption
  ( long "fin"
  <> short 'i'
  <> metavar "FIN"
  <> help "Encrypt the content of the file 'FIN'"
  )

stdinParser :: Parser Input
stdinParser = pure StdIn

inputParser :: Parser Input
inputParser = messageParser
             <|> fileParser
             <|> stdinParser

decryptParser :: Parser Bool
decryptParser = switch
  ( long "decrypt"
  <> short 'd'
  <> help "Decrypt the text rather than encrypting it"
  )

argsParser :: Parser Args
argsParser = Args
           <$> cipherParser
           <*> inputParser
           <*> decryptParser

-- | Command line parser for 'cipher'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "cipher - Command line utility to encrypt short ASCII texts"
  <> progDesc "Encrypt a text with a given cipher" )