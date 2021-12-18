module Parsers where

import Prelude
import Control.Monad (join)
import Data.Maybe (fromJust)
import Polysemy
import Polysemy.Megaparsec
import Text.Megaparsec hiding
  ( Parsec, ParsecT
  , runParser, runParser'
  , parse, parseMaybe
  , runParserT, runParserT'
  )
import Text.Megaparsec.Char
import Control.Applicative.Combinators hiding (some, many)
import Data.Void

type Parser r a = Parsec Void String r a

nat :: (Integral i, Read i) => Parser r i
nat = do
  s <- some digitChar
  return (read s)

withSign :: (Integral i) => Parser r i -> Parser r i
withSign p = do
  sign <- do
    s <- optional $ oneOf @[] "+-"
    case s of
      Just '-' -> return (-1)
      _ -> return 1
  nat <- p
  return (sign * nat)

int :: (Integral i, Read i) => Parser r i
int = withSign nat
