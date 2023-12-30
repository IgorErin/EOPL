module Tests.Let.Eval(tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Let.Lang (run)
import Let.Eval (
    ExpVal,
    num, 
    bool_, false_, true_,
    cons, nil)

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
    ("< (6, 3)", false_),
    
    ("nil", nil),
    ("cons 3 1", cons (num 3) (num 1)),
    ("Cdr cons 3 1", num 1),
    ("Car cons 3 1", num 3),
    ("let x = cons 1 nil in cons nil x", cons nil (cons (num 1) nil))]

tests :: TestTree
tests = 
    testGroup "Eval" $
    map (\ (str, expected) -> 
        let x = run str in 
        testCase str $ x @?= expected) 
        cases 
