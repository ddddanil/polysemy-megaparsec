{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE IncoherentInstances #-}
module Suite.Parser (testTree) where

import Prelude
import Polysemy
import Polysemy.State
import Polysemy.Check
import Polysemy.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Parsers

testTree :: TestTree
testTree = testGroup "Polysemy.Megaparsec"
  [ testProperty "Parse number" parseNumberTest
  ]


parseNumber :: forall r. String -> Sem r (Maybe Integer)
parseNumber = parseMaybe @r int

parseNumberLaw
  :: forall s r f effs res
   . ( res ~ Maybe s
     , effs ~ '[State Bool] -- synthesized effects for testing
     , Arbitrary s
     , Eq s, Show s
     , s ~ Integer -- Specify result
     , ArbitraryEff effs r
     , Members effs r
     , (forall z. Eq z => Eq (f z))
     , (forall z. Show z => Show (f z))
     )
  => (forall a. Sem r (res, a) -> IO (f (res, a)))
  -> Property
parseNumberLaw = prepropLaw @effs program
  where
    program :: Gen (Sem r res, Sem r res)
    program = do
      i <- arbitrary @Integer
      pure (parseNumber (show i)
           , pure (Just i))

parseNumberTest :: Property
parseNumberTest = parseNumberLaw interpreter
  where
    interpreter = pure . pure @Maybe . run . evalState True

