{-# LANGUAGE LambdaCase #-}

-- |
-- Module: Expressionism.Arithmetic
module Expressionism.Arithmetic where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)


-- | Syntax for arithmetic expressions
data AExpr a
    = ANum Int
    | APlus (AExpr a) (AExpr a)
    | AMult (AExpr a) (AExpr a)
    | AIdent a
    | ALet a (AExpr a) (AExpr a)
    deriving (Eq, Show)


-- | Instructions for calculating arithmetic instructions on a stack
data AInst
    = IPush Int
    | IPlus
    | IMult
    deriving (Eq, Show)


-- | Compile an arithmetic expression into a program that evaluates it
compile :: (Eq a, Ord a, Show a) => AExpr a -> [AInst]
compile = inner Map.empty
    where
    inner env = \case
        ANum n -> [IPush n]
        APlus x y -> inner env x <> inner env y <> [IPlus]
        AMult x y -> inner env x <> inner env y <> [IMult]
        AIdent x -> inner env . fromMaybe (error $ "Unknown identifier: " <> show x) $ Map.lookup x env
        ALet x e b -> inner (Map.insert x e env) b


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
interpret :: (Ord a, Eq a, Show a) => AExpr a -> Int
interpret = inner Map.empty
    where
    inner env = \case
        ANum n -> n
        APlus x y -> inner env x + inner env y
        AMult x y -> inner env x * inner env y
        AIdent x -> inner env $ fromMaybe (error $ "Unknown identifier: " <> show x) $ Map.lookup x env
        ALet x e body -> inner (Map.insert x e env) body
