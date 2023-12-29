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

trueExp :: ExpVal
trueExp = ExpBool True 
falseExp :: ExpVal
falseExp = ExpBool False

getNum :: String -> ExpVal -> Int 
getNum _ (ExpInt n) = n 
getNum place _      = error $ "getNum faild in " ++ place  

getBool :: String -> ExpVal -> Bool 
getBool _ (ExpBool b) = b 
getBool place _       = error $ "getBool faild in " ++ place 

valueOf :: Env -> Expr -> ExpVal
valueOf env (Ident name) = fromMaybe  (error $ "not found: " ++ name) (Map.lookup name env)
valueOf _ (Num n) = ExpInt n
valueOf env (Diff left right) = 
    let leftValue = getNum "left diff" $ valueOf env left 
        rightValue = getNum "right diff" $ valueOf env right 
    in ExpInt (leftValue - rightValue)
valueOf env (IsZero e) = 
    let value = getNum "isZero" $ valueOf env e 
    in if value == 0 then trueExp else falseExp  
valueOf env (Let name value body) =
    let env' = Map.insert name (valueOf env value) env
    in valueOf env' body 
valueOf env (IfElse cond t e) = 
    let cond' = getBool "IfElse" $ valueOf env cond 
    in if cond' then valueOf env t else valueOf env e 

run = valueOf Map.empty 
