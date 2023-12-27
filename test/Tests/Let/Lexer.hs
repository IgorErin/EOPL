module Tests.Let.Lexer(tests) where 

import qualified Let.Lexer as L 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Let.Lexer (Token(..), alexScanTokens)

cases :: [(String, [L.Token])]
cases = [
    ("let in x y lfaef 3252435 ( ) - ,",
     [Let,In,Ident "x",Ident "y",Ident "lfaef",Num 3252435,LParent,RParent,Minus,Comma])
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = alexScanTokens str in 
        testCase str $ x @?= expected) 
        cases 
