{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              (replicateM)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.State  (evalStateT)
import           Data.Functor.Identity      (runIdentity)
import           Test.Tasty
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Expressionism              (Expr (..), Name, preludeDefs)
import           Expressionism.Arithmetic   (AExpr (..))
import qualified Expressionism.Arithmetic   as A
import qualified Expressionism.Bool         as B
import           Expressionism.Compiler     (initMachine)
import           Expressionism.GraphReducer (machineStep, runExecT)
import qualified Expressionism.List         as L
import           Expressionism.Machine      (GNode, GraphNode (..),
                                             MachineError)


main :: IO ()
main = defaultMain $ testGroup "tests" [preludeTests, boolTests, arithTests]


runMachine x y =
    fmap last
    . runIdentity
    . runExecT (replicateM 2000 machineStep)
    $ initMachine x y

runMachine0 = flip runMachine preludeDefs
runMachineList = flip runMachine $ [L.coreReverse, L.coreTake, L.coreSum] <> preludeDefs
runMachineBool = flip runMachine B.preludeBool


retVal :: Int -> Either MachineError (Maybe GNode)
retVal i = Right . Just $ GNodeNum i


coreTrue :: GNode
coreTrue = GNodeData 1 []


preludeTests :: TestTree
preludeTests = testGroup "prelude"
    [ testCase "I should compute the identity" $
        runMachine0 (Ap (Ident "I") (Nmbr 1)) @?= retVal 1

    , testCase "K should project the first arg" $
        runMachine0 (Ap (Ap (Ident "K") (Nmbr 1)) (Nmbr 2)) @?= retVal 1

    , testCase "K1 should project the second arg" $
        runMachine0 (Ap (Ap (Ident "K1") (Nmbr 1)) (Nmbr 2)) @?= retVal 2

    , testCase "let bindings" $ do
        runMachine0 letProgramA @?= retVal 1
        runMachine0 letProgramB @?= retVal 1

    , testCase "letrec bindings" $
        runMachine0 letRecProgram @?= retVal 1

    , testCase "primitives" $ do
        runMachine0 primProgramA @?= retVal 7
        runMachine0 primProgramB @?= retVal 6
        runMachine0 primProgramC @?= Right (Just coreTrue)

    , testCase "case" $ do
        runMachine0 caseProgram @?= retVal 3
        runMachine0 caseProgram2 @?= retVal 13

    , testCase "bool" $
        runMachine0 boolProgram @?= retVal 100

    , testCase "lambdas" $ do
        runMachine0 lamProgram @?= retVal 1
        runMachine0 lamProgram2 @?= retVal 10
        runMachine0 lamProgram3 @?= retVal 13

    , testCase "lists" $ do
        runMachineList listProgramA @?= retVal 15
        runMachineList listProgramB @?= retVal 6
        runMachineList listProgramC @?= retVal 12

    ]

    where

    -- let x = K 1 2 in K x 3
    letProgramA = Let False [("x", Ap (Ap (Ident "K") (Nmbr 1)) (Nmbr 2))] $
        Ap (Ap (Ident "K") (Ident "x")) (Nmbr 3)

    -- let x = K 1 2 in let y = K x 4 in K1 5 y
    letProgramB =
        Let False [("x", (Ident "K" `Ap` Nmbr 1) `Ap` Nmbr 2)]
        . Let False [("y", (Ident "K" `Ap` Ident "x") `Ap` Nmbr 4)]
        $ (Ident "K1" `Ap` Nmbr 5) `Ap` Ident "y"

    -- letrec x = K 1 y
    --        y = K x
    -- in y 2
    letRecProgram =
        Let True [ ("x", (Ident "K" `Ap` Nmbr 1) `Ap` Ident "y")
                 , ("y", Ident "K" `Ap` Ident "x")
                 ]
        $ Ident "y" `Ap` Nmbr 2

    -- + 3 4
    primProgramA = Ident "+" `Ap` Nmbr 3 `Ap` Nmbr 4

    -- let x = + 1 2 in + x 3
    primProgramB =
        Let False [("x", Ident "+" `Ap` Nmbr 1 `Ap` Nmbr 2)]
        $ Ident "+" `Ap` Ident "x" `Ap` Nmbr 3

    -- > 3 0
    primProgramC = Ident ">" `Ap` Nmbr 3 `Ap` Nmbr 0

    caseProgram = Case (Constr 0 1 `Ap` Nmbr 3)
        [ (0, ["x"], Ident "x")
        , (1, ["y"], Ident "y")
        ]

    caseProgram2 = Let False [("z", caseProgram)] $
        Case (Constr 1 2 `Ap` Nmbr 10 `Ap` Nmbr 11)
        [ (0, [], Nmbr 101)
        , (1, ["x", "y"], Ident "+" `Ap` Ident "x" `Ap` Ident "z")
        , (2, ["x"], Ident "*" `Ap` Ident "x" `Ap` Ident "x")
        ]

    boolProgram = Ident "if" `Ap` Ident "true" `Ap` Nmbr 100 `Ap` Nmbr 0

    -- (\x -> x) 1
    lamProgram = Lam ["x"] (Ident "x") `Ap` Nmbr 1

    -- let x = \y -> + y 1
    --     u = 9
    -- in x u
    lamProgram2 =
        Let False [ ("x", Lam ["y"] $ Ident "+" `Ap` Ident "y" `Ap` Nmbr 1)
                  , ("u", Nmbr 9)
                  ]
        $ Ident "x" `Ap` Ident "u"

    -- letrec f = \x y -> + x (* 2 y)
    --        g = \f z -> f 3 z
    --        y = f 1 2
    -- in g f y
    lamProgram3 =
        Let True [ ("f", Lam ["x", "y"] $ Ident "+" `Ap` Ident "x" `Ap` (Ident "*" `Ap` Nmbr 2 `Ap` Ident "y"))
                 , ("g", Lam ["f", "z"] $ Ident "f" `Ap` Nmbr 3 `Ap` Ident "z")
                 , ("y", Ident "f" `Ap` Nmbr 1 `Ap` Nmbr 2)
                 ]
        $ Ident "g" `Ap` Ident "f" `Ap` Ident "y"


    listProgramA = Ident "sum" `Ap` L.toCoreList [1..5]
    listProgramB = Ident "sum" `Ap` (Ident "take" `Ap` Nmbr 3 `Ap` L.toCoreList [1..5])
    listProgramC = Ident "sum" `Ap` (Ident "take" `Ap` Nmbr 3 `Ap` (Ident "reverse" `Ap` L.toCoreList [1..5]))



boolTests :: TestTree
boolTests = testGroup "bool"
    [ testCase "true" $ runMachineBool (B.toNumber B.true) @?= retTrue
    , testCase "false" $ runMachineBool (B.toNumber B.false) @?= retFalse
    , testCase "not false" $ runMachineBool (B.toNumber $ B.apNot B.false) @?= retTrue
    , testCase "true and false" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.false) @?= retFalse
    , testCase "true and true" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.true) @?= retTrue
    , testCase "true or false" $ runMachineBool (B.toNumber $ B.true `B.apOr` B.false) @?= retTrue
    , testCase "false or false" $ runMachineBool (B.toNumber $ B.false `B.apOr` B.false) @?= retFalse
    ]

    where
    retTrue = retVal 1
    retFalse = retVal 0


arithTests :: TestTree
arithTests = testGroup "arithmetic"
    [ testCase "interpret" $ A.interpret exprA @?= 20
    , testCase "compile and run" $ A.run (A.compile exprA) @?= 20
    , testCase "let bindings" $ do
        A.interpret exprB @?= 26
        A.run (A.compile exprB) @?= 26
    ]

    where

    exprA :: AExpr Name
    exprA = ANum 1 `APlus` ANum 3 `AMult` ANum 5

    exprB :: AExpr Name
    exprB =
        ALet "x" (ANum 3 `APlus` ANum 1) $
        ALet "y" (ANum 4 `AMult` AIdent "x") $
        AIdent "y" `APlus` ANum 10
