module Let.Eval (ExpVal(..), num, bool_, getNum, getBool, run) where 

import qualified Data.Map as Map 
import Data.Maybe (fromMaybe)

import Let.Ast as A 

data ExpVal = ExpInt Int | ExpBool Bool deriving (Show, Eq) 

num :: Int -> ExpVal 
num = ExpInt 

bool_ :: Bool -> ExpVal
bool_ = ExpBool 

type Env = Map.Map Ident ExpVal 

getNum :: String -> ExpVal -> Int 
getNum _ (ExpInt n) = n 
getNum place _      = error $ "getNum faild in " ++ place  

getBool :: String -> ExpVal -> Bool 
getBool _ (ExpBool b) = b 
getBool place _       = error $ "getBool faild in " ++ place 

binOp :: Env -> (Int -> Int -> Int) -> Expr -> Expr -> ExpVal
binOp env op left right = 
    let leftValue = getNum "left op" $ valueOf env left 
        rightValue = getNum "right op" $ valueOf env right 
    in ExpInt (leftValue `op` rightValue)

binPred :: Env -> (Int -> Int -> Bool) -> Expr -> Expr -> ExpVal 
binPred env p left right = 
    let leftValue = getNum "left pred" $ valueOf env left 
        rightValue = getNum "right pred" $ valueOf env right 
    in ExpBool (leftValue `p` rightValue)

valueOf :: Env -> Expr -> ExpVal
valueOf env (Ident name) = fromMaybe  (error $ "not found: " ++ name) (Map.lookup name env)
valueOf _ (Num n) = ExpInt n
valueOf env (Arithm op left right) = case op of 
    Add -> binOp env (+) left right 
    Sub -> binOp env (-) left right 
    Mul -> binOp env (*) left right 
    Div -> binOp env div left right
valueOf env (Pred p left right) = case p of
    Eq -> binPred env (==) left right  
    NEq -> binPred env (/=) left right  
    Gt -> binPred env (>) left right  
    Ge -> binPred env (>=) left right  
    Lt -> binPred env (<) left right  
    Le -> binPred env (<=) left right  
valueOf env (Let name value body) =
    let env' = Map.insert name (valueOf env value) env
    in valueOf env' body 
valueOf env (IfElse cond t e) = 
    let cond' = getBool "IfElse" $ valueOf env cond 
    in if cond' then valueOf env t else valueOf env e
    
run :: Expr -> ExpVal
run = valueOf Map.empty 
