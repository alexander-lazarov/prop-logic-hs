module PropositionalLogic
( Name
, Prop (..)
, Environment
, varValue
, evalWithEnv
, allVars
, bind
, allBools
, allEnvs
, isTautology
, isSatisfiable
, isContradiction
) where

type Name = String
type Environment = [(Name, Bool)]

data Prop = Const Bool
          | Var Name
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Implies Prop Prop
    deriving Eq

instance Show Prop where
    show (Const b)          = if b then "T" else "F"
    show (Var x)            = x
    show (Not fi)           = '¬' : show fi
    show (fi `And` psi)     = show fi ++ " & " ++ show psi
    show (fi `Or` psi)      = "(" ++ show fi ++ " ∨ " ++ show psi ++ ")"
    show (fi `Implies` psi) = "(" ++ show fi ++ " → " ++ show psi ++ ")"

infixl 9 `And`
infixl 8 `Or`
infixr 7 `Implies`

varValue :: Environment -> Name -> Bool
varValue [] _               = False
varValue ((name, val):xs) n = if n == name then val else varValue xs n

evalWithEnv :: Environment -> Prop -> Bool
evalWithEnv _   (Const a)     = a
evalWithEnv env (Var n)       = varValue env n
evalWithEnv env (Not x)       = not $ evalWithEnv env x
evalWithEnv env (And x y)     = (evalWithEnv env x) && (evalWithEnv env y)
evalWithEnv env (Or x y)      = (evalWithEnv env x) || (evalWithEnv env y)
evalWithEnv env (Implies x y) = (not xv) || (xv && yv)
    where xv = evalWithEnv env x
          yv = evalWithEnv env y

allVars :: Prop -> [Name]
allVars (Const _)     = []
allVars (Var x)       = [x]
allVars (Not x)       = allVars x
allVars (And x y)     = allVars x ++ allVars y
allVars (Or x y)      = allVars x ++ allVars y
allVars (Implies x y) = allVars x ++ allVars y


bind :: [Name] -> [Bool] -> Environment
bind [] _          = []
bind _ []          = []
bind (x:xs) (y:ys) = (x, y):(bind xs ys)

allBools :: Integer -> [[Bool]]
allBools 0 = []
allBools 1 = [[False], [True]]
allBools n = [ x:xs | x <- [False, True], xs <- allBools $ n -1 ]

allEnvs :: [Name] -> [Environment]
allEnvs []     = []
allEnvs (x:[]) = [[(x, False)], [(x, True)]]
allEnvs (x:xs) = [ e:es | e <- [(x, False), (x, True)], es <- allEnvs xs ]

allPropEnvs :: Prop -> [Environment]
allPropEnvs = allEnvs . allVars

isTautology :: Prop -> Bool
isTautology p = all (`evalWithEnv` p) (allPropEnvs p)

isSatisfiable :: Prop -> Bool
isSatisfiable p = any (`evalWithEnv` p) (allPropEnvs p)

isContradiction :: Prop -> Bool
isContradiction p = isTautology $ Not p

