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
   '+'      { L.Plus }
   '*'      { L.Mul }    
   '/'      { L.Div }    
   isZero   { L.IsZero }
   if       { L.If }
   then     { L.Then }
   else     { L.Else }
   ','      { L.Comma }
   ident    { L.Ident $$ }
   num      { L.Num $$ } 
   neg      { L.Neg }

%%

-- Program :: { A.Expr }
Program : Expr                      { $1}

-- Expr :: { A.Expr }
Expr 
    : num                           { Num $1 }
    | '-' '(' Expr ',' Expr ')'     { Diff $3 $5 }
    | '+' '(' Expr ',' Expr ')'     { Sum $3 $5 }
    | '*' '(' Expr ',' Expr ')'     { Mul $3 $5 }
    | '/' '(' Expr ',' Expr ')'     { Div $3 $5 }
    | isZero Expr                   { IsZero $2 }
    | if Expr then Expr else Expr   { IfElse $2 $4 $6 }
    | ident                         { Ident $1 }
    | let ident '=' Expr in Expr    { Let $2 $4 $6 }
    | neg Expr                      { Neg $2 }
    | '(' Expr ')'                  { $2 }

{
parseError :: [L.Token] -> a 
parseError _ = error "Parse error"
}
