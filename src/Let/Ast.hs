module Let.Ast (Expr (..), Ident) where 

type Ident = String  

data Expr = 
    Num Int 
    | Diff Expr Expr 
    | IsZero Expr 
    | IfElse Expr Expr Expr 
    | Ident Ident 
    | Let Ident Expr Expr 
    deriving (Show, Eq)