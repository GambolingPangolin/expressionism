{-# LANGUAGE OverloadedStrings #-}

module Expressionism.Bool where

import           Expressionism (CoreExpr, CoreSC, Expr (..))


preludeBool :: [CoreSC]
preludeBool = [trueSC, falseSC, boolAnd, boolOr, boolNot, boolIf]


trueSC :: CoreSC
trueSC = ("true", ["x", "y"], Ident "x")


true :: CoreExpr
true = Ident "true"


falseSC :: CoreSC
falseSC = ("false", ["x", "y"], Ident "y")


false :: CoreExpr
false = Ident "false"


boolAnd :: CoreSC
boolAnd =
    ( "and"
    , ["u", "v", "x", "y"]
    , Ident "u" `Ap` (Ident "v" `Ap` Ident "x" `Ap` Ident "y") `Ap` Ident "y"
    )

apAnd :: CoreExpr -> CoreExpr -> CoreExpr
apAnd x = Ap (Ident "and" `Ap` x)


boolOr :: CoreSC
boolOr =
    ( "or"
    , ["u", "v", "x", "y"]
    , Ident "u" `Ap` Ident "x" `Ap` Ident "v" `Ap` Ident "x" `Ap` Ident "y"
    )

apOr :: CoreExpr -> CoreExpr -> CoreExpr
apOr x = Ap (Ident "or" `Ap` x)


boolNot :: CoreSC
boolNot =
    ( "not"
    , ["u", "x", "y"]
    , Ident "u" `Ap` Ident "y" `Ap` Ident "x"
    )

apNot :: CoreExpr -> CoreExpr
apNot = Ap $ Ident "not"


boolIf :: CoreSC
boolIf =
    ( "if"
    , ["u", "x", "y"]
    , Ident "u" `Ap` Ident "x" `Ap` Ident "y"
    )


apIf :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
apIf c x = Ap (Ident "if" `Ap` Ident "c" `Ap` Ident "x")


toNumber :: CoreExpr -> CoreExpr
toNumber b = b `Ap` Nmbr 1 `Ap` Nmbr 0
