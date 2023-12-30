module Let.Ast (
    Expr (..), Ident, Predicate(..), Arithm(..),
    sub, add, mul, div_, neg,
    eq, neq, ge, le, gt, lt, isZero) where 

type Ident = String 

data Predicate = Eq | NEq | Gt | Ge | Lt | Le deriving (Show, Eq) 

data Arithm = Add | Sub | Mul | Div deriving (Show, Eq)

data Expr = 
    Num Int 
    -- arithm 
    | Arithm Arithm Expr Expr
    -- predicates
    | Pred Predicate Expr Expr 
    | IfElse Expr Expr Expr 
    | Ident Ident 
    | Let Ident Expr Expr 
    deriving (Show, Eq)

---- Arithm ----
sub :: Expr -> Expr -> Expr
sub = Arithm Sub 

add :: Expr -> Expr -> Expr
add = Arithm Add 

mul :: Expr -> Expr -> Expr
mul = Arithm Mul 

div_ :: Expr -> Expr -> Expr
div_ = Arithm Div 

neg :: Expr -> Expr 
neg = Arithm Sub (Num 0)

-------- Predicates --------

eq :: Expr -> Expr -> Expr 
eq = Pred Eq 

neq :: Expr -> Expr -> Expr 
neq = Pred NEq 

gt :: Expr -> Expr -> Expr 
gt = Pred Gt 

lt :: Expr -> Expr -> Expr 
lt = Pred Lt 

ge :: Expr -> Expr -> Expr  
ge = Pred Ge 

le :: Expr -> Expr -> Expr 
le = Pred Le 

isZero :: Expr -> Expr
isZero = Pred Eq (Num 0)
