module Tests.Let.Parser (tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Let.Frontend as F (run) 
import Let.Ast as A 

cases :: [(String, A.Expr)]
cases = [
    ("let x = - (1, 3) in 4", Let "x" (Diff (Num 1) (Num 3)) (Num 4)),
    ("let x = 7 \
    \ in let y = 2    \
    \  in let y = let x = -(x,1) \
    \ in -(x,y) \
    \ in -(-(x,8), y)", 
    Let "x" (Num 7) 
    (Let "y" (Num 2)
     (Let "y" 
        (Let "x" (Diff (Ident "x") (Num 1)) 
        (Diff (Ident "x") (Ident "y")))
        (Diff (Diff (Ident "x") (Num 8)) (Ident "y")))))
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = F.run str in 
        testCase str $ x @?= expected) 
        cases 
