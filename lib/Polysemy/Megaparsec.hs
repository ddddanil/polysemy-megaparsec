module Polysemy.Megaparsec (
  Parsec
, lift
) where

import Polysemy
import qualified Control.Monad.Trans.Class as T
import qualified Text.Megaparsec as MP

type Parsec e s r a = MP.ParsecT e s (Sem r) a

lift :: Sem r a -> Parsec e s r a
lift = T.lift


