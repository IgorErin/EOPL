module Tests.Let.Parser (tests) where 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Let.Lang as Lang (parse) 
import Let.Ast as A 

cases :: [(String, A.Expr)]
cases = [
    ("let x = - (1, 3) in 4", Let "x" (sub (Num 1) (Num 3)) (Num 4)),
    ("let x = IsZero x in if x then 0 else 1",
      Let "x" (eq (Num 0) (Ident "x")) (IfElse (Ident "x") (Num 0) (Num 1))),
    ("let x = 7 \
    \ in let y = 2    \
    \ in let y = let x = -(x,1) \
    \ in -(x,y) \
    \ in -(-(x,8), y)", 
    Let "x" (Num 7) 
    (Let "y" (Num 2)
     (Let "y" 
        (Let "x" (sub (Ident "x") (Num 1)) 
        (sub (Ident "x") (Ident "y")))
        (sub (sub (Ident "x") (Num 8)) (Ident "y"))))),
    ("minus 4", sub (Num 0) (Num 4)),
    ("minus let x = 1 in x", sub (Num 0) (Let "x" (Num 1) (Ident "x"))),

    ("+ (4, 1)", add (Num 4) (Num 1)),
    ("- (4, 1)", sub (Num 4) (Num 1)),
    ("* (4, 1)", mul (Num 4) (Num 1)),
    ("/ (4, 1)", div_ (Num 4) (Num 1)),

    ("== (3, 1)", eq (Num 3) (Num 1)),
    ("/= (3, 1)", neq (Num 3) (Num 1)),
    ("> (3, 1)", gt (Num 3) (Num 1)),
    (">= (3, 1)", ge (Num 3) (Num 1)),
    ("<= (3, 1)", le (Num 3) (Num 1)),
    ("< (3, 1)", lt (Num 3) (Num 1)),
    
    ("cons 2 3", cons (Num 2) (Num 3)),
    ("cons 2 nil", cons (Num 2) Nil),
    ("nil", Nil),
    ("Cdr nil", cdr Nil),
    ("Car nil", car Nil),
    ("let x = cons y 4 in Cdr x", 
        Let "x" (Bin Cons (Ident "y") (Num 4)) 
            (Un Cdr (Ident "x"))),
                
    ("[]", Nil),
    ("[1, 2, 3, 4]", cons (Num 1) (cons (Num 2) (cons (Num 3) (cons (Num 4) nil))) )]

tests :: TestTree
tests = 
    testGroup "Parsing" $
    map (\ (str, expected) -> 
        let x = Lang.parse str in 
        testCase str $ x @?= expected) 
        cases 
