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

   '['      { L.LSquare }
   ']'      { L.RSquare }

   '-'      { L.Sub }
   '+'      { L.Add }
   '*'      { L.Mul }    
   '/'      { L.Div }

   '>'      { L.Gt } 
   '>='     { L.Ge } 
   '<'      { L.Lt } 
   '<='     { L.Le } 
   '=='     { L.Eq } 
   '/='     { L.NEq } 

   isZero   { L.IsZero }
   if       { L.If }
   then     { L.Then }
   else     { L.Else }

   ','      { L.Comma }

   ident    { L.Ident $$ }
   num      { L.Num $$ } 

   neg      { L.Neg }
-- List 
   cons     { L.Cons }
   nil      { L.Nil }
   cdr      { L.Cdr }
   car      { L.Car }
   isNil    { L.IsNil }

   cond     { L.Cond }
   end      { L.End }
   '->'     { L.Arrow }
   '|'      { L.VBar }
%%

Program :: { A.Expr }
Program : Expr                      { $1}

Expr :: { A.Expr }
Expr 
    : num                           { Num $1 }
    | '-' '(' Expr ',' Expr ')'     { A.sub $3 $5 }
    | '+' '(' Expr ',' Expr ')'     { A.add $3 $5 }
    | '*' '(' Expr ',' Expr ')'     { A.mul $3 $5 }
    | '/' '(' Expr ',' Expr ')'     { A.div_ $3 $5 }

    | '==' '(' Expr ',' Expr ')'    { A.eq $3 $5 }
    | '/=' '(' Expr ',' Expr ')'    { A.neq $3 $5 }
    | '>' '(' Expr ',' Expr ')'     { A.gt $3 $5 }
    | '>=' '(' Expr ',' Expr ')'    { A.ge $3 $5 }
    | '<' '(' Expr ',' Expr ')'     { A.lt $3 $5 }
    | '<=' '(' Expr ',' Expr ')'    { A.le $3 $5 }

    | isZero Expr                   { A.isZero $2 }
    | if Expr then Expr else Expr   { IfElse $2 $4 $6 }
    | ident                         { Ident $1 }
    | let ident '=' Expr in Expr    { Let $2 $4 $6 }
    | neg Expr                      { A.neg $2 }
    | '(' Expr ')'                  { $2 }

    | cons Expr Expr                { A.cons $2 $3 }
    | nil                           { A.nil }
    | isNil Expr                    { A.isNil $2 }          
    | car Expr                      { A.car $2 }
    | cdr Expr                      { A.cdr $2 }
    | List                          { $1 }
    | cond Variants end             { A.variants $ reverse $2 }

Variants :: { [(A.Expr, A.Expr)] }
Variants 
   : Variants '|' Variant           { $3 : $1 }
   | opt('|') Variant               { [$2] }

Variant :: {(A.Expr, A.Expr)} 
Variant : Expr '->' Expr            { ($1, $3) }

List :: { A.Expr }
List : 
   '[' ']'                          { A.nil }
   | '[' SubList ']'                { foldr A.cons A.nil $ reverse $2 }

SubList :: { [A.Expr] }
SubList 
   : SubList ',' Expr               { $3 : $1 } 
   | Expr                           { [$1] }

-------------------- Helpers -----------------

opt(p) 
   : p                              { Just $1 }
   | {- empty -}                    { Nothing }  

{
parseError :: [L.Token] -> a 
parseError _ = error "Parse error"
}
