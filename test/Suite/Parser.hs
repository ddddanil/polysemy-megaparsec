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
  , unitTest "parse keyword choice wrong" test_parseChoice_wrong
  ]

test_parseNumber :: UnitTest
test_parseNumber = runTestAuto $ do
  i <- embed . generate $ arbitrary @Integer
  res <- parseMaybe int (pack . show $ i)
  res === Just i

test_parseChoice :: UnitTest
test_parseChoice = runTestAuto $ do
  ss <- embed . generate $ arbitrary @[Text]
  correct <- embed . generate $ elements ss
  runInputConst ss $ do
    res <- parseMaybe keywordsChoice correct
    res === Just correct

test_parseChoice_wrong :: UnitTest
test_parseChoice_wrong = runTestAuto $ do
  ss <- embed . generate $ arbitrary @[Text]
  correct <- embed . generate $ arbitrary @Text `suchThat` (`notElem` ss)
  runInputConst ss $ do
    res <- parseMaybe keywordsChoice correct
    res === Nothing

