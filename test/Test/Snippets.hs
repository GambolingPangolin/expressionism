{-# LANGUAGE OverloadedStrings #-}

module Test.Snippets where


import           Expressionism      (CoreExpr, Expr (..))
import qualified Expressionism.List as L


-- let x = K 1 2 in K x 3
letProgramA :: CoreExpr
letProgramA = Let False [("x", Ap (Ap (Ident "K") (Nmbr 1)) (Nmbr 2))] $
    Ap (Ap (Ident "K") (Ident "x")) (Nmbr 3)


-- let x = K 1 2 in let y = K x 4 in K1 5 y
letProgramB :: CoreExpr
letProgramB =
    Let False [("x", (Ident "K" `Ap` Nmbr 1) `Ap` Nmbr 2)]
    . Let False [("y", (Ident "K" `Ap` Ident "x") `Ap` Nmbr 4)]
    $ (Ident "K1" `Ap` Nmbr 5) `Ap` Ident "y"


-- letrec x = K 1 y
--        y = K x
-- in y 2
letRecProgram :: CoreExpr
letRecProgram =
    Let True [ ("x", (Ident "K" `Ap` Nmbr 1) `Ap` Ident "y")
                , ("y", Ident "K" `Ap` Ident "x")
                ]
    $ Ident "y" `Ap` Nmbr 2


-- + 3 4
primProgramA :: CoreExpr
primProgramA = Ident "+" `Ap` Nmbr 3 `Ap` Nmbr 4


-- let x = + 1 2 in + x 3
primProgramB :: CoreExpr
primProgramB =
    Let False [("x", Ident "+" `Ap` Nmbr 1 `Ap` Nmbr 2)]
    $ Ident "+" `Ap` Ident "x" `Ap` Nmbr 3


-- > 3 0
primProgramC :: CoreExpr
primProgramC = Ident ">" `Ap` Nmbr 3 `Ap` Nmbr 0


-- case C_0^1 3 of
--      C_0 x -> x ;
--      C_1 y -> y
caseProgram :: CoreExpr
caseProgram = Case (Constr 0 1 `Ap` Nmbr 3)
    [ (0, ["x"], Ident "x")
    , (1, ["y"], Ident "y")
    ]

-- let z = caseProgram1 in
-- case C_1^2 10 11 of
--      C_0 -> 101 ;
--      C_1 x y -> + x z ;
--      C_2 x -> * x x
caseProgram2 :: CoreExpr
caseProgram2 = Let False [("z", caseProgram)] $
    Case (Constr 1 2 `Ap` Nmbr 10 `Ap` Nmbr 11)
    [ (0, [], Nmbr 101)
    , (1, ["x", "y"], Ident "+" `Ap` Ident "x" `Ap` Ident "z")
    , (2, ["x"], Ident "*" `Ap` Ident "x" `Ap` Ident "x")
    ]


-- if true 100 0
boolProgram :: CoreExpr
boolProgram = Ident "if" `Ap` Ident "true" `Ap` Nmbr 100 `Ap` Nmbr 0


-- (\x -> x) 1
lamProgram :: CoreExpr
lamProgram = Lam ["x"] (Ident "x") `Ap` Nmbr 1


-- let x = \y -> + y 1
--     u = 9
-- in x u
lamProgram2 :: CoreExpr
lamProgram2 =
    Let False [ ("x", Lam ["y"] $ Ident "+" `Ap` Ident "y" `Ap` Nmbr 1)
                , ("u", Nmbr 9)
                ]
    $ Ident "x" `Ap` Ident "u"


-- letrec f = \x y -> + x (* 2 y)
--        g = \f z -> f 3 z
--        y = f 1 2
-- in g f y
lamProgram3 :: CoreExpr
lamProgram3 =
    Let True [ ("f", Lam ["x", "y"] $ Ident "+" `Ap` Ident "x" `Ap` (Ident "*" `Ap` Nmbr 2 `Ap` Ident "y"))
                , ("g", Lam ["f", "z"] $ Ident "f" `Ap` Nmbr 3 `Ap` Ident "z")
                , ("y", Ident "f" `Ap` Nmbr 1 `Ap` Nmbr 2)
                ]
    $ Ident "g" `Ap` Ident "f" `Ap` Ident "y"


listProgramA :: CoreExpr
listProgramA = Ident "sum" `Ap` L.toCoreList [1..5]


listProgramB :: CoreExpr
listProgramB = Ident "sum" `Ap` (Ident "take" `Ap` Nmbr 3 `Ap` L.toCoreList [1..5])


listProgramC :: CoreExpr
listProgramC = Ident "sum" `Ap` (Ident "take" `Ap` Nmbr 3 `Ap` (Ident "reverse" `Ap` L.toCoreList [1..5]))
