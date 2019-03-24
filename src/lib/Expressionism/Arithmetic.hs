-- |
-- Module: Expressionism.Arithmetic
module Expressionism.Arithmetic where


-- | Syntax for arithmetic expressions
data AExpr
    = ANum Int
    | APlus AExpr AExpr
    | AMult AExpr AExpr
    deriving (Eq, Show)


-- | Instructions for calculating arithmetic instructions on a stack
data AInst
    = IPush Int
    | IPlus
    | IMult
    deriving (Eq, Show)


-- | Compile an arithmetic expression into a program that evaluates it
compile :: AExpr -> [AInst]
compile (ANum n)    = [IPush n]
compile (APlus x y) = compile x <> compile y <> [IPlus]
compile (AMult x y) = compile x <> compile y <> [IMult]


-- | Run an arithmetic program
run :: [AInst] -> Int
run = inner []
    where

    inner ns (IPush n : is)     = inner (n:ns) is
    inner (n0:n1:ns) (IPlus:is) = inner (n0+n1:ns) is
    inner (n0:n1:ns) (IMult:is) = inner (n0*n1:ns) is
    inner [n] []                = n
    inner _ _                   = error "Invalid program"


-- | Interpret an arithmetic expression directly
interpret :: AExpr -> Int
interpret (ANum n)    = n
interpret (APlus x y) = interpret x + interpret y
interpret (AMult x y) = interpret x * interpret y
