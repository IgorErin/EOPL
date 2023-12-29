module Tests.Let.Parser (tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Let.Lang as Lang (parse) 
import Let.Ast as A 

cases :: [(String, A.Expr)]
cases = [
    ("let x = - (1, 3) in 4", Let "x" (Diff (Num 1) (Num 3)) (Num 4)),
    ("let x = IsZero x in if x then 0 else 1",
      Let "x" (IsZero (Ident "x")) (IfElse (Ident "x") (Num 0) (Num 1))),
    ("let x = 7 \
    \ in let y = 2    \
    \ in let y = let x = -(x,1) \
    \ in -(x,y) \
    \ in -(-(x,8), y)", 
    Let "x" (Num 7) 
    (Let "y" (Num 2)
     (Let "y" 
        (Let "x" (Diff (Ident "x") (Num 1)) 
        (Diff (Ident "x") (Ident "y")))
        (Diff (Diff (Ident "x") (Num 8)) (Ident "y"))))),
    ("minus 4", Neg (Num 4)),
    ("minus let x = 1 in x", Neg (Let "x" (Num 1) (Ident "x")))
    ]

tests :: TestTree
tests = 
    testGroup "Parsing" $
    map (\ (str, expected) -> 
        let x = Lang.parse str in 
        testCase str $ x @?= expected) 
        cases 
