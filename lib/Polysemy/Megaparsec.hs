module Polysemy.Megaparsec (
  Parsec
, lift
, runParser, runParser', runParserMaybe
) where

import Prelude
import Polysemy
import qualified Control.Monad.Trans.Class as T
import qualified Text.Megaparsec as MP
import Data.Either.Combinators

type Parsec e s r a = MP.ParsecT e s (Sem r) a

lift :: Sem r a -> Parsec e s r a
lift = T.lift

runParser :: Parsec e s r a -> String -> s -> Sem r (Either (MP.ParseErrorBundle s e) a)
runParser = MP.runParserT

runParser' :: Parsec e s r a -> MP.State s e -> Sem r (MP.State s e, Either (MP.ParseErrorBundle s e) a)
runParser' = MP.runParserT'

runParserMaybe :: (Ord e, MP.Stream s) => Parsec e s r a -> s -> Sem r (Maybe a)
runParserMaybe p s = rightToMaybe <$> runParser (p <* MP.eof) "" s

