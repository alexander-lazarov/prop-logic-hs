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
, semanticallyImplies
, semanticallyEquivalent
, isAxiom
, modusPonens
, proofFrom
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

semanticallyImplies :: Prop -> Prop -> Bool
semanticallyImplies x y = all implies $ allEnvs vars
  where vars = allVars x ++ allVars y
        implies = \e -> evalWithEnv e $ x `Implies` y

semanticallyEquivalent :: Prop -> Prop -> Bool
semanticallyEquivalent x y = (semanticallyImplies x y) && (semanticallyImplies y x)

isAxiom :: Prop -> Bool
-- NOT-1
isAxiom ((a `Implies` b)
            `Implies`
            ((c `Implies` Not d) `Implies` Not e))       = a == c && a == e && b == d
-- THEN-2
isAxiom ((a `Implies` b `Implies` c)
            `Implies`
            ((d `Implies` e) `Implies` (f `Implies` g))) = a == d && a == f && b == e && c == g

-- OR-3
isAxiom ((a `Implies` b)
             `Implies`
             ((c `Implies` d)
                 `Implies` ((e `Or` f) `Implies` g)))    = ((a == e && c == f) || (a == f && c == e))
                                                               && b == d && b == g
-- AND-3
isAxiom (a `Implies` b `Implies` c `And` d)              = (b == c && a == d) || (a == c && b == d)
-- NOT-2
isAxiom (a `Implies` (Not b `Implies` _))                = a == b
-- THEN-1
isAxiom (a `Implies` _ `Implies` c)                      = a == c
-- AND-1 and AND-2
isAxiom (a `And` b `Implies` c)                          = a == c || b == c
-- OR-1 and OR-2
isAxiom (a `Implies` b `Or` c)                           = a == b || a == c
-- NOT 3
isAxiom (a `Or` Not b)                                   = a == b
isAxiom _                                                = False

modusPonens :: Prop -> Prop -> Prop -> Bool
modusPonens a (b `Implies` c) d = a == b && c ==d
modusPonens _ _ _               = False

proofFrom :: [Prop] -> [Prop] -> Bool
proofFrom xs ys = proofFromHelper xs $ reverse ys

proofFromHelper :: [Prop] -> [Prop] -> Bool
proofFromHelper _  []     = True
proofFromHelper xs (y:ys) = (elem y xs || isAxiom y || isModusPonens) && proofFromHelper xs ys
  where
    p               = ys ++ xs
    modusPonensPair = \(a, b) -> modusPonens a b y
    isModusPonens   = any modusPonensPair [(a, b) | a <- p, b <- p]

