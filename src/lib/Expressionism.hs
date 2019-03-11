-- |
-- Module: Expressionism
--
-- Library for working with the /Core/ language


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Expressionism where


import           Data.String (IsString)
import           Data.Word   (Word64)


newtype Name = Name String
    deriving (Eq, IsString)

-- | The type of Core expressions parameterized over binders
data Expr a
    = Var a
    | Nmbr Int
    | Constr Word64 Word64
    | Ap (Expr a) (Expr a)
    | Let Bool [(a, Expr a)] (Expr a)
    | Case (Expr a) [(Word64, [a], Expr a)]
    | Lam [a] (Expr a)
    deriving Eq


type CoreExpr = Expr Name


isAtomic :: Expr a -> Bool
isAtomic (Var _)  = True
isAtomic (Nmbr _) = True
isAtomic _        = False


type CoreProgram = [ (Name, [Name], Expr Name) ]


preludeDefs :: CoreProgram
preludeDefs =
    [ ("I", ["x"], Var "x")
    , ("K", ["x", "y"], Var "x")
    , ("K1", ["x", "y"], Var "y")
    , ("S", ["f", "g", "x"], (Var "f" `Ap` Var "x") `Ap` (Var "g" `Ap` Var "x"))
    , ("compose", ["f", "g", "x"], Var "f" `Ap` (Var "g" `Ap` Var "x"))
    , ("twice", ["f"], (Var "compose" `Ap` Var "f") `Ap` Var "f")
    ]
