{-# LANGUAGE QuantifiedConstraints #-}
module Suite.Parser (testTree) where

import Prelude
import Data.Text
import Polysemy
import Polysemy.Input
import Polysemy.Check
import Polysemy.Test
import Polysemy.Megaparsec
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((===))
import Test.QuickCheck.Instances.Text ()
import Parsers

testTree :: TestTree
testTree = testGroup "Polysemy.Megaparsec"
  [ unitTest "parse number" test_parseNumber
  , unitTest "parse keyword choice via Input" test_parseChoice
  ]

test_parseNumber :: UnitTest
test_parseNumber = runTestAuto $ do
  i <- embedFinal . generate $ arbitrary @Integer
  res <- parseMaybe int (pack . show $ i)
  res === Just i

test_parseChoice :: UnitTest
test_parseChoice = runTestAuto $ do
  ss <- embedFinal . generate $ arbitrary @[Text]
  correct <- embedFinal . generate $ elements ss
  runInputConst ss $ do
    res <- parseMaybe keywordsChoice correct
    res === Just correct

