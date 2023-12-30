module Let.Ast (
    Expr (..), Ident, BinOp(..), UnOp(..),
    sub, add, mul, div_, neg,
    eq, neq, ge, le, gt, lt, isZero,
    nil, car, cdr, cons, isNil) where 

type Ident = String 

data BinOp = 
    -- Num Arithm 
    Add 
    | Sub
    | Mul 
    | Div 
    -- Predicates
    | Eq 
    | NEq 
    | Gt 
    | Ge 
    | Lt 
    | Le 
    -- List 
    | Cons 
    deriving (Eq, Show)

data UnOp = 
    IsNil 
    | Cdr 
    | Car 
    deriving (Eq, Show)

data Expr = 
    Num Int 
    -- arithm 
    | Bin BinOp Expr Expr
    -- predicates
    | Un UnOp Expr 
    | IfElse Expr Expr Expr 
    | Ident Ident 
    | Let Ident Expr Expr 
    | Nil
    deriving (Show, Eq)

---- BinOp  ----

sub :: Expr -> Expr -> Expr
sub = Bin Sub 

add :: Expr -> Expr -> Expr
add = Bin Add 

mul :: Expr -> Expr -> Expr
mul = Bin Mul 

div_ :: Expr -> Expr -> Expr
div_ = Bin Div 

neg :: Expr -> Expr 
neg = Bin Sub (Num 0)

--------- Un op -----------

cdr :: Expr -> Expr 
cdr = Un Cdr 

car :: Expr -> Expr 
car = Un Car

-------- Predicates --------

eq :: Expr -> Expr -> Expr 
eq = Bin Eq 

neq :: Expr -> Expr -> Expr 
neq = Bin NEq 

gt :: Expr -> Expr -> Expr 
gt = Bin Gt 

lt :: Expr -> Expr -> Expr 
lt = Bin Lt 

ge :: Expr -> Expr -> Expr  
ge = Bin Ge 

le :: Expr -> Expr -> Expr 
le = Bin Le 

----------- Un Pred -------

isNil :: Expr -> Expr 
isNil = Un IsNil 

isZero :: Expr -> Expr
isZero = Bin Eq (Num 0)

-------- Constructors ----

nil :: Expr 
nil = Nil

cons :: Expr -> Expr -> Expr 
cons = Bin Cons  