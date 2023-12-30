module Let.Eval (ExpVal(..),
    num, getNum,
    bool_, true_, false_, getBool,
    cons, nil,
    run) where 

import qualified Data.Map as Map 
import Data.Maybe (fromMaybe, isNothing)

import Let.Ast as A (Ident, UnOp(..),BinOp(..), Expr(..))  

data ExpList = 
    ExpCons ExpVal ExpVal
    | ExpNil  
    deriving (Eq, Show)

data ExpVal =
    ExpInt Int 
    | ExpBool Bool 
    | ExpList ExpList 
    deriving (Show, Eq) 

------- Num --------
num :: Int -> ExpVal 
num = ExpInt 

getNum :: String -> ExpVal -> Int 
getNum _ (ExpInt n) = n 
getNum place _      = error $ "getNum faild in " ++ place  

----- Bool -------
bool_ :: Bool -> ExpVal
bool_ = ExpBool 

true_ :: ExpVal 
true_ = bool_ True 

false_ :: ExpVal 
false_ = bool_ False

getBool :: String -> ExpVal -> Bool 
getBool _ (ExpBool b) = b 
getBool place _       = error $ "getBool faild in " ++ place 

----- List -------
cons :: ExpVal -> ExpVal -> ExpVal   
cons car cdr = ExpList $ ExpCons car cdr  

nil :: ExpVal
nil = ExpList ExpNil 

getList :: String -> ExpVal -> Maybe (ExpVal, ExpVal)
getList _ (ExpList list) = case list of 
    ExpCons left right -> Just (left, right)
    ExpNil             -> Nothing 
getList place _ = error $ "getList faild in " ++ place 

type Env = Map.Map Ident ExpVal 

binOp :: Env -> (Int -> Int -> Int) -> Expr -> Expr -> ExpVal
binOp env op left right = 
    let leftValue = getNum "left op" $ valueOf env left 
        rightValue = getNum "right op" $ valueOf env right 
    in ExpInt $ leftValue `op` rightValue

binPred :: Env -> (Int -> Int -> Bool) -> Expr -> Expr -> ExpVal 
binPred env p left right = 
    let leftValue = getNum "left pred" $ valueOf env left 
        rightValue = getNum "right pred" $ valueOf env right 
    in ExpBool $ leftValue `p` rightValue

valueOf :: Env -> Expr -> ExpVal
valueOf env (Ident name) = fromMaybe  (error $ "not found: " ++ name) (Map.lookup name env)
valueOf _ (Num n) = ExpInt n
valueOf env (Bin op left right) = case op of 
    --- Arithm
    Add -> binOp env (+) left right 
    Sub -> binOp env (-) left right 
    Mul -> binOp env (*) left right 
    Div -> binOp env div left right
    --- Predicates 
    Eq -> binPred env (==) left right  
    NEq -> binPred env (/=) left right  
    Gt -> binPred env (>) left right  
    Ge -> binPred env (>=) left right  
    Lt -> binPred env (<) left right  
    Le -> binPred env (<=) left right  
    Cons -> 
        -- rework duplication 
        let leftValue = valueOf env left 
            rightValue = valueOf env right 
        in ExpList $ ExpCons leftValue rightValue
valueOf env (Un p e) = case p of
    IsNil -> bool_ . isNothing $ l
    Car   -> maybe (error "Car failed") fst l 
    Cdr   -> maybe (error "Cdr failed") snd l 
    where l = getList "UnOp" $ valueOf env e
valueOf env (Let name value body) =
    let env' = Map.insert name (valueOf env value) env
    in valueOf env' body 
valueOf env (IfElse cond t e) = 
    let cond' = getBool "IfElse" $ valueOf env cond 
    in if cond' then valueOf env t else valueOf env e
valueOf _ Nil = ExpList ExpNil    
valueOf env (Variants ls) = 
    foldr (\ (cond, body) right -> 
        let condValue = getBool "Cond" $ valueOf env cond 
        in if condValue then valueOf env body else right) 
    (error "No cond sat") ls 

run :: Expr -> ExpVal
run = valueOf Map.empty 
