-- This is a sample white-box test suite. You can do pretty much anything here,
-- but tests that don't depend on internal details of your implementation
-- should go into BlackBox.hs

import Defs
import ParserImpl
import ResolverImpl
import CoderImpl

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Tests" [testPStringType]

testPStringType = testGroup "Test Type" 
    [ testCase "parser" $ parseStringType "a" @?= Right PTVar "a"]