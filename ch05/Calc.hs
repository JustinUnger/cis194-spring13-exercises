{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

-- Excerise1: an evaluator for ExprT

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

-- Exercise 2: an evaluator for String

evalStr :: String -> Maybe Integer
evalStr x = case (parseExp Lit Add Mul x) of
               Nothing -> Nothing
               Just x -> Just (eval x)
               

-- Exercise 3: create Expr type class and create instance of Expr for ExprT

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id


-- Exercise 4: make more instances of Expr for various types

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

--instance Expr Integer where
--  lit x = x
--  mul x y = x * y
--  add x y = x + y

-- here's a nicer version. we need the parens around the opertors so they won't be eval'd as infix

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

-- note use of partially applied function below in lit

instance Expr Bool where
  lit = (> 0)
  mul = (&&)
  add = (||)

instance Expr MinMax where
  lit = MinMax
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where 
  lit x = Mod7 $ x `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7


-- exercise 5

instance Expr S.Program where
   lit x = [S.PushI x]
   mul x y = x ++ y ++ [S.Mul]
   add x y = x ++ y ++ [S.Add]

testSVM = testExp :: Maybe S.Program

compile :: String -> Maybe S.Program
compile = parseExp lit mul add 

-- exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VVar String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
       deriving (Show, Eq)

instance Expr VarExprT where
   lit = VLit
   mul = VMul
   add = VAdd

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  mul = foomul
  add = fooadd

foolit :: Integer -> (M.Map String Integer -> Maybe Integer)
foolit x _ = Just x

foomul :: (M.Map String Integer -> Maybe Integer) 
       -> (M.Map String Integer -> Maybe Integer)
       -> (M.Map String Integer -> Maybe Integer)
foomul x y map = helper (x map) (y map) where
   helper :: Maybe Integer -> Maybe Integer -> Maybe Integer
   helper Nothing _ = Nothing
   helper _ Nothing = Nothing
   helper (Just x) (Just y) = Just (x*y)

fooadd :: (M.Map String Integer -> Maybe Integer) 
       -> (M.Map String Integer -> Maybe Integer)
       -> (M.Map String Integer -> Maybe Integer)
fooadd x y map = helper (x map) (y map) where
   helper :: Maybe Integer -> Maybe Integer -> Maybe Integer
   helper Nothing _ = Nothing
   helper _ Nothing = Nothing
   helper (Just x) (Just y) = Just (x+y)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

--- goof around

varStore :: M.Map String Integer
varStore = M.fromList vars

vars :: [(String, Integer)]
vars = [("x",3)]

