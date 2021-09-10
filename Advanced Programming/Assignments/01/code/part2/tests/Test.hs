-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Test.Tasty
import Test.Tasty.HUnit

-- import Control.Monad
-- import Data.Semigroup
-- import Data.Text (Text)
-- import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text
-- import Text.Printf

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [showExpTests, evalSimpleTests]

showExpTests = testGroup "showExp tests"
  [ testCase "showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) should show 2*(3+4)" $
      showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` "2*(3+4)" @?= EQ
  , testCase "showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) should show (2*3)+4" $
      showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` "(2*3)+4" @?= EQ
  ]

evalSimpleTests = testGroup "evalSimple tests"
  [ testCase "2*(3+4) = 14" $
      evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` 14 @?= EQ
  , testCase "(2*3)+4 = 10" $
      evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` 10 @?= EQ
  ]
-- https://www.youtube.com/watch?v=PGsDvgmZF7A
-- readExamples :: IO [(Text, Text)]
-- readExamples = 
--   mapM asPair =<< Text.lines <$> Text.readFile "tests/showExp.csv"
--     where 
--       asPair line =
--         case Text.splitOn "," line of
--           [input, expected] -> pure (input, expected)
--           _ -> fail ("Invalid example line: " <> Text.unpack line)