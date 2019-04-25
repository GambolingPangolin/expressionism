{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad              (replicateM)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.State  (evalStateT)
import           Data.Functor.Identity      (runIdentity)
import qualified Data.Text                  as T
import           Test.Tasty
import           Test.Tasty.HUnit           (testCase, (@?=))

import           Expressionism              (Expr (..), Name, preludeDefs,
                                             pretty)
import           Expressionism.Arithmetic   (AExpr (..))
import qualified Expressionism.Arithmetic   as A
import qualified Expressionism.Bool         as B
import           Expressionism.Compiler     (initMachine)
import           Expressionism.GraphReducer (machineStep, runExecT)
import           Expressionism.Lexer        (scanTokens)
import qualified Expressionism.List         as L
import           Expressionism.Machine      (GNode, GraphNode (..),
                                             MachineError)
import           Expressionism.Parser       (parseExpression)

import           Test.Snippets


main :: IO ()
main = defaultMain $ testGroup "tests" [preludeTests, boolTests, arithTests, parsingTests]


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


parsingTests :: TestTree
parsingTests = testGroup "parsing"
    [ testCase "example A" $
        parse sourceA @?= treeA

    , testCase "example B" $
        parse sourceB @?= treeB

    , testCase "example C" $
        parse sourceC @?= treeC

    ]

    where

    parse = parseExpression . scanTokens

    sourceA = "f x y = { + x y }"
    treeA = [("f", ["x", "y"], Ident "+" `Ap` Ident "x" `Ap` Ident "y")]
    sourceB = "f x y = { let z = * 3 x in case y of C_0 u -> z; C_1 v -> + v z }"
    treeB = [("f", ["x", "y"], Let False [("z", Ident "*" `Ap` Nmbr 3 `Ap` Ident "x")] $ Case (Ident "y") [(0, ["u"], Ident "z"), (1, ["v"], Ident "+" `Ap` Ident "v" `Ap` Ident "z")])]
    sourceC = T.unpack $ "f = { " <> pretty lamProgram <> "}"
    treeC = [("f", [], lamProgram)]
