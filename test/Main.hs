{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Expressionism              (Expr (..), Name, preludeDefs)
import           Expressionism.Arithmetic   (AExpr (..))
import qualified Expressionism.Arithmetic   as A
import qualified Expressionism.Bool         as B
import           Expressionism.GraphReducer (MachineError, execBounded,
                                             initMachine, result)


main :: IO ()
main = defaultMain $ testGroup "tests" [preludeTests, boolTests, arithTests]


runMachine x y = fmap result . execBounded 100 $ initMachine x y

runMachine0 = flip runMachine preludeDefs
runMachineBool = flip runMachine B.preludeBool


retVal :: Int -> Either MachineError (Maybe Int)
retVal i = Right $ Just i


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
