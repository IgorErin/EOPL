module Main where 

import Test.Tasty

import qualified Tests.Let.Lexer as L (tests)
import qualified Tests.Let.Parser as P (tests)
import qualified Tests.Let.Eval as E (tests)

tests :: TestTree
tests = testGroup "Main" [L.tests, P.tests, E.tests]

main :: IO ()
main = defaultMain tests
