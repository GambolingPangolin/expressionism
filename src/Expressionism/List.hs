{-# LANGUAGE OverloadedStrings #-}

module Expressionism.List where


import           Expressionism (CoreExpr, CoreSC, Expr (..))


coreNil :: CoreExpr
coreNil = Constr 0 0

coreCons :: CoreExpr -> CoreExpr -> CoreExpr
coreCons h t = Constr 1 2 `Ap` h `Ap` t


toCoreList :: [Int] -> CoreExpr
toCoreList = foldr coreCons coreNil . fmap Nmbr


coreSum :: CoreSC
coreSum =
    ( "sum"
    , ["xs"]
    , Case (Ident "xs") [ (0, [], Nmbr 0)
                        , (1, ["h", "t"], Ident "+" `Ap` Ident "h" `Ap` (Ident "sum" `Ap` Ident "t"))
                        ]
    )


coreReverse :: CoreSC
coreReverse =
    ( "reverse"
    , ["xs"]
    , Let True [("go", goBody)] $ Ident "go" `Ap` coreNil `Ap` Ident "xs"
    )
    where
    goBody = Lam ["zs", "xs'"] $
        Case (Ident "xs'") [ (0, [], Ident "zs")
                           , (1, ["h", "t"], Ident "go" `Ap` coreCons (Ident "h") (Ident "zs") `Ap` Ident "t")
                           ]


coreTake :: CoreSC
coreTake =
    ( "take"
    , ["n", "xs"]
    , Ident "if" `Ap` (Ident ">" `Ap` Ident "n" `Ap` Nmbr 0) `Ap` takeMore `Ap` coreNil
    )
    where
    takeMore =
        Case (Ident "xs") [ (0, [], Ident "xs")
                          , (1, ["h", "t"], coreCons (Ident "h") (Ident "take" `Ap` nMinusOne `Ap` Ident "t"))
                          ]
    nMinusOne = Ident "-" `Ap` Ident "n" `Ap` Nmbr 1
