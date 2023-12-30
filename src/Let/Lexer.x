{
    module Let.Lexer where 
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  "let"                 { \_ -> Let }
  "in"                  { \_ -> In }
  "IsZero"              { \_ -> IsZero }
  $digit+               { \s -> Num (read s) }
  "="                   { \_ -> Assign }
  "-"                   { \_ -> Sub }
  "+"                   { \_ -> Add }
  "*"                   { \_ -> Mul }
  "/"                   { \_ -> Div }
  ">"                   { \_ -> Gt }
  ">="                  { \_ -> Ge }
  "<"                   { \_ -> Lt }
  "<="                  { \_ -> Le }
  "=="                  { \_ -> Eq }
  "/="                  { \_ -> NEq }
  "minus"               { \_ -> Neg }
  "("                   { \_ -> LParent }
  ")"                   { \_ -> RParent}
  ","                   { \_ -> Comma }
  "if"                  { \_ -> If }
  "then"                { \_ -> Then }
  "else"                { \_ -> Else }
  @ident                { \s -> Ident s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = 
  -- key words
  Point
  | If 
  | Then 
  | Else
  | IsZero
  | Assign 
  | Let 
  | In 
  | LParent 
  | RParent
-- Values
  | Num Int
  | Ident String 
-- binop 
  | Sub 
  | Add 
  | Mul 
  | Div
-- unop 
  | Neg 
  | Comma 
-- Predicates 
  | Gt 
  | Ge 
  | Lt 
  | Le 
  | Eq 
  | NEq 
  deriving (Eq, Show)
}