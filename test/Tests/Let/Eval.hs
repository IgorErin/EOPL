module Tests.Let.Eval(tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Let.Lang (run)
import Let.Eval (num, bool_, ExpVal)

true_ :: ExpVal 
true_ = bool_ True

false_ :: ExpVal
false_ = bool_ False

cases :: [(String, ExpVal)]
cases = [
    ("let x = IsZero 0 in if x then 1 else 2", num 1),
    ("let x = 3 in let y = 4 in -(x, y)", num (-1)),

    ("IsZero 0", true_),
    ("IsZero 342", false_),

    ("minus 4", num (-4)), 
    ("minus - (0, 5432)", num 5432),

    ("+ (6, 3)", num 9),
    ("- (6, 3)", num 3),
    ("* (6, 3)", num 18),
    ("/ (6, 3)", num 2),

    ("== (6, 3)", false_),
    ("/= (6, 3)", true_),
    (">= (6, 3)", true_),
    ("> (6, 3)", true_),
    ("<= (6, 3)", false_),
    ("< (6, 3)", false_)]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = run str in 
        testCase str $ x @?= expected) 
        cases 
