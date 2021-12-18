import Prelude
import Test.Tasty
import qualified Suite.Parser as SP

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ SP.testTree
  ]
