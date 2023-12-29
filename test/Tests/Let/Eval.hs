module Tests.Let.Eval where 

import qualified Let.Lexer as L 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Let.Lang (run)
import Let.Eval (num, bool_, ExpVal)

cases :: [(String, ExpVal)]
cases = [
    ("let x = IsZero 0 in if x then 1 else 2", num 1),
    ("let x = 3 in let y = 4 in -(x, y)", num (-1)),
    ("IsZero 0", bool_ True),
    ("IsZero 342", bool_ False)]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = run str in 
        testCase str $ x @?= expected) 
        cases 
