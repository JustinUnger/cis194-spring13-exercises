> {-# LANGUAGE TypeSynonymInstances #-}

> import Parser
> import StackVM

> testProg = [PushB True, PushI 3, PushI 6, Mul]

> class Expr a where
>   lit :: Expr a => Integer -> a
>   mul :: Expr a => a -> a -> a
>   add :: Expr a => a -> a -> a

> instance Expr StackVM.Program where
>  lit x = undefined
>  mul x y = undefined
>  add x y = undefined
