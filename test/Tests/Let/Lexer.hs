module Tests.Let.Lexer(tests) where 

import Let.Lexer 

import Test.Tasty 
import Test.Tasty.HUnit ((@?=), testCase)

import Let.Lang (lexing)

cases :: [(String, [Token])]
cases = [
    ("let in x y lfaef 3252435 ( ) - ,",
     [Let,In,Ident "x",Ident "y",Ident "lfaef",Num 3252435,LParent,RParent,Minus,Comma])
    ]

tests :: TestTree
tests = 
    testGroup "Lexing" $
    map (\ (str, expected) -> 
        let x = lexing str in 
        testCase str $ x @?= expected) 
        cases 
