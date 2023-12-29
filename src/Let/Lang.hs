module Let.Lang (lexing, parse, run ) where 

import qualified Let.Ast as A  

import qualified Let.Parser as P 
import qualified Let.Lexer as L
import qualified Let.Eval as E

lexing :: String -> [L.Token]
lexing = L.alexScanTokens 

parse :: String -> A.Expr
parse = P.calc . lexing

run :: String -> E.ExpVal
run = E.run . parse  
     