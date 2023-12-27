module Let.Frontend (run) where 

import qualified Let.Ast as A  

import qualified Let.Parser as P 
import qualified Let.Lexer as L

run :: String -> A.Expr
run = P.calc . L.alexScanTokens
     