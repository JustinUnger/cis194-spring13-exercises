> -- {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

> import ExprT
> import Parser
> import StackVM

> eval :: ExprT -> Integer
> eval (ExprT.Lit x) = x
> eval (ExprT.Add a1 a2) = (eval a1) + (eval a2) 
> eval (ExprT.Mul a1 a2) = (eval a1) * (eval a2)

> evalStr :: String -> Maybe Integer
> evalStr xs = case (parseExp ExprT.Lit ExprT.Add ExprT.Mul xs) of
>                     Nothing      -> Nothing
>                     Just x       -> Just (eval x)

> class Expr a where
>   lit :: Expr a => Integer -> a
>   mul :: Expr a => a -> a -> a
>   add :: Expr a => a -> a -> a

> instance Expr ExprT where
>  lit x = ExprT.Lit x
>  mul x y =  ExprT.Mul x y
>  add x y =  ExprT.Add x y

> reify :: ExprT -> ExprT
> reify = id


> newtype MinMax = MinMax Integer deriving (Eq, Show)
> newtype Mod7   = Mod7 Integer deriving (Eq, Show)

> testExp :: Expr a => Maybe a
> testExp = parseExp lit add mul "(3 * -4) + 5"

> testInteger = testExp :: Maybe Integer
> testBool    = testExp :: Maybe Bool
> testMM      = testExp :: Maybe MinMax
> testSat     = testExp :: Maybe Mod7

> instance Expr Integer where
>   lit x = eval (ExprT.Lit x)
>   mul x y = eval (ExprT.Mul (Lit x) (Lit y))
>   add x y = eval (ExprT.Add (Lit x) (Lit y))

> instance Expr Bool where
>   lit x = if x <= 0 then False else True
>   mul x y = x && y
>   add x y = x || y

> instance Expr MinMax where
>   lit x = MinMax x
>   mul (MinMax x) (MinMax y) = MinMax (min x y)
>   add (MinMax x) (MinMax y) = MinMax (max x y)

> instance Expr Mod7 where
>   lit x = Mod7 (x `mod` 7)
>   mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
>   add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

> testLitExp :: Expr a => Maybe a
> testLitExp = parseExp lit add mul "7"

exercise 5

> instance Expr Program where
>   lit x = undefined
>   mul x y = undefined
>   add x y = undefined

