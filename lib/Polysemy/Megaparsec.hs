module Polysemy.Megaparsec (
  Parsec
, lift
, runParser, runParser', runParserMaybe
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

type Parsec e s r a = MP.ParsecT e s (Sem r) a

lift :: Sem r a -> Parsec e s r a
lift = T.lift

runParser :: (Subsume rP rM) => Parsec e s rP a -> String -> s -> Sem rM (Either (MP.ParseErrorBundle s e) a)
runParser p n s = subsume_ $ MP.runParserT p n s

runParser' :: (Subsume rP rM) => Parsec e s rP a -> MP.State s e -> Sem rM (MP.State s e, Either (MP.ParseErrorBundle s e) a)
runParser' p s = subsume_ $ MP.runParserT' p s

runParserMaybe :: (Subsume rP rM, Ord e, MP.Stream s) => Parsec e s rP a -> s -> Sem rM (Maybe a)
runParserMaybe p s = rightToMaybe <$> runParser (p <* MP.eof) "" s

runParserState :: (Subsume rP rM, Member (State (MP.State s e)) rM) => Parsec e s rP a -> Sem rM (Either (MP.ParseErrorBundle s e) a)
runParserState p = do
  s <- get
  (s', a) <- runParser' p s
  put s'
  return a

runParserWithError :: (Subsume rP rM, Member (Error (MP.ParseErrorBundle s e)) rM) => Parsec e s rP a -> String -> s -> Sem rM a
runParserWithError p n s = runParser p n s >>= fromEither

