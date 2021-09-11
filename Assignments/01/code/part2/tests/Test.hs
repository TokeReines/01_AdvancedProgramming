-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.Tasty
import Test.Tasty.HUnit

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [showExpTests, evalSimpleTests]

showExpTests = testGroup "showExp tests"
  [ testCase "showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) should show (2*(3+4))" $ showExp (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` "(2*(3+4))" @?= EQ
  , testCase "showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) should show ((2*3)+4)" $ showExp (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` "((2*3)+4)" @?= EQ
  , testCase "showExp (Exp (Cst 2) (Cst 3)) should show (2^3)" $ showExp (Exp (Cst 2) (Cst 3)) `compare` "(2^3)" @?= EQ
  -- , testCase "showExp  should show (2*3)^(2+3)+3" $
  --     showExp  `compare` "(2*3)^(2+3)" @?= EQ
  ]

evalSimpleTests = testGroup "evalSimple tests"
  [ testCase "2*(3+4) = 14" $ evalSimple (Mul (Cst 2) (Add (Cst 3) (Cst 4))) `compare` 14 @?= EQ
  , testCase "(2*3)+4 = 10" $ evalSimple (Add (Mul (Cst 2) (Cst 3)) (Cst 4)) `compare` 10 @?= EQ
  , testCase "Division by zero should throw an error" $ assertException error (evalSimple (Div (Cst 2) (Cst 0)) )
  ]

-- https://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
-- https://www.youtube.com/watch?v=PGsDvgmZF7A
-- readExamples :: IO [(Text, Text)]
-- readExamples = 
--   mapM asPair =<< Text.lines <$> Text.readFile "tests/showExp.csv"
--     where 
--       asPair line =
--         case Text.splitOn "," line of
--           [input, expected] -> pure (input, expected)
--           _ -> fail ("Invalid example line: " <> Text.unpack line)