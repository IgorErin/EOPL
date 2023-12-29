{
module Let.Parser where 

import qualified Let.Lexer as L 
import Let.Ast as A    
}

%name calc 
%tokentype {L.Token}
%error {parseError }

%token
   let      { L.Let }
   '='      { L.Assign }
   in       { L.In }
   '('      { L.LParent }
   ')'      { L.RParent }
   '-'      { L.Minus }
   isZero   { L.IsZero }
   if       { L.If }
   then     { L.Then }
   else     { L.Else }
   ','      { L.Comma }
   ident    { L.Ident $$ }
   num      { L.Num $$ } 

%%

-- Program :: { A.Expr }
Program : Expr                      { $1}

-- Expr :: { A.Expr }
Expr 
    : num                           { Num $1 }
    | '-' '(' Expr ',' Expr ')'     { Diff $3 $5 }
    | isZero Expr                   { IsZero $2 }
    | if Expr then Expr else Expr   { IfElse $2 $4 $6 }
    | ident                         { Ident $1 }
    | let ident '=' Expr in Expr    { Let $2 $4 $6 }

{
parseError :: [L.Token] -> a 
parseError _ = error "Parse error"
}