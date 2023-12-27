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
  "="                   {\_ -> Assign}
  "-"                   { \_ -> Minus}
  "("                   {\_ -> LParent }
  ")"                   { \_ -> RParent}
  ","                   { \_ -> Comma }
  "if"                  { \_ -> If }
  "then"                { \_ -> Then }
  "else"                { \_ -> Else }
  @ident                {\s -> Ident s}

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
  | Minus 
  | Comma 
  deriving (Eq, Show)
}