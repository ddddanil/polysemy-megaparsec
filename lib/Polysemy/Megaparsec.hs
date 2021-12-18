{-|
Module: Polysemy.Megaparsec
Description: Adapters for running 'Text.Megaparsec' parsers in polysemy code
Copyright: (c) Danil Doroshin, 2021
Maintainer: ddddanil5555@gmail.com

This module provides adapters for the 'Text.Megaparsec.ParsecT' monad inside 'Polysemy'.

The nature of parsers makes them badly suited for an effect: they are very local, favour
being run explicitly, and they feel most comfortable to use at the head of a monad stack.
The stack tail, however, can greatly benefit from the effects-based approach.

The names in this module will collide with the ones from 'Text.Megaparsec'. It also contains
runners that are logically replaced with the adapters form this module. You can hide
them like this:

@
import Text.Megaparsec hiding
  ( Parsec, ParsecT
  , runParser, runParser'
  , parse, parseMaybe
  , runParserT, runParserT'
  )
@
-}
module Polysemy.Megaparsec (
-- * Parsec type
  Parsec
, lift
-- * Adapters
--
-- ** Basic
, runParser, runParser', runParserMaybe
-- ** Effectful
, runParserState, runParserWithError
) where

import Prelude
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Internal
import qualified Control.Monad.Trans.Class as T
import qualified Text.Megaparsec as MP
import Data.Either.Combinators

-- | Type synonym for easy access to 'Sem' effects
--
-- It's common to have a type synonym with monomorphised error and stream variables
--
-- @
-- type Parser = Parsec Void Text
-- @
type Parsec e s r a = MP.ParsecT e s (Sem r) a

-- | Type-constrained version of 'Control.Monad.Trans.Class.lift'
lift :: Sem r a -> Parsec e s r a
lift = T.lift

-- | @runParser p source input@ runs parser @p@ on @input@. @source@ is only used in error messages and may be empty.
-- returns a potentially failed result
--
-- Using 'runParserWithError' can allow for more convenient error handling.
runParser :: (Subsume rP rM) => Parsec e s rP a -> String -> s -> Sem rM (Either (MP.ParseErrorBundle s e) a)
runParser p n s = subsume_ $ MP.runParserT p n s

-- | The funcion is similar to 'runParser' with the difference that it gives a direct access to the underlying state.
-- It is useful with combination of 'Text.Megaparsec.getParserState' and 'Text.Megaparsec.updateParserState'.
--
-- If you want to handle state externally, consider using 'runParserState'
runParser' :: (Subsume rP rM) => Parsec e s rP a -> MP.State s e -> Sem rM (MP.State s e, Either (MP.ParseErrorBundle s e) a)
runParser' p s = subsume_ $ MP.runParserT' p s

-- | @parseMaybe p input@ runs the parser @p@ on input and returns the result inside 'Just' on success and 'Nothing' on failure.
-- This function also parses 'MP.eof', so if the parser doesn't consume all of its input, it will fail.
--
-- The function is supposed to be useful for lightweight parsing, where error messages (and thus file names) are not important
-- and entire input should be consumed. For example, it can be used for parsing of a single number according to a specification of its format.
runParserMaybe :: (Subsume rP rM, Ord e, MP.Stream s) => Parsec e s rP a -> s -> Sem rM (Maybe a)
runParserMaybe p s = rightToMaybe <$> runParser (p <* MP.eof) "" s

-- | This adapter allows threading of external 'State' effect into the parser.
--
-- __Note__: the parser still uses its own State monad inside. They are only synchronised upon entry and exit of this adapter.
--
-- @since 0.2
runParserState :: (Subsume rP rM, Member (State (MP.State s e)) rM) => Parsec e s rP a -> Sem rM (Either (MP.ParseErrorBundle s e) a)
runParserState p = do
  s <- get
  (s', a) <- runParser' p s
  put s'
  return a

-- | This adapter allows to embed the resulting 'Either' into a global 'Error' effect.
--
-- __Note__: the parser still uses its own error handling monad inside. They are only synchronised upon the exit of this adapter.
--
-- @since 0.2
runParserWithError :: (Subsume rP rM, Member (Error (MP.ParseErrorBundle s e)) rM) => Parsec e s rP a -> String -> s -> Sem rM a
runParserWithError p n s = runParser p n s >>= fromEither

