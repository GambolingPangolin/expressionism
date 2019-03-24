{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit           (testCase, (@=?))

import           Expressionism              (Expr (..), preludeDefs)
import qualified Expressionism.Bool         as B
import           Expressionism.GraphReducer (execBounded, initMachine, result, MachineError)


main :: IO ()
main = defaultMain $ testGroup "tests" [preludeTests, boolTests]


runMachine x y = fmap result . execBounded 100 $ initMachine x y

runMachine0 = flip runMachine preludeDefs
runMachineBool = flip runMachine B.preludeBool


retVal :: Int -> Either MachineError (Maybe Int)
retVal i = Right $ Just i


preludeTests :: TestTree
preludeTests = testGroup "prelude"
    [ testCase "I should compute the identity" $
        runMachine0 (Ap (Ident "I") (Nmbr 1)) @=? retVal 1

    , testCase "K should project the first arg" $
        runMachine0 (Ap (Ap (Ident "K") (Nmbr 1)) (Nmbr 2)) @=? retVal 1

    , testCase "K1 should project the second arg" $
        runMachine0 (Ap (Ap (Ident "K1") (Nmbr 1)) (Nmbr 2)) @=? retVal 2

    ]


boolTests :: TestTree
boolTests = testGroup "bool"
    [ testCase "true" $ runMachineBool (B.toNumber B.true) @=? retTrue
    , testCase "false" $ runMachineBool (B.toNumber B.false) @=? retFalse
    , testCase "not false" $ runMachineBool (B.toNumber $ B.apNot B.false) @=? retTrue
    , testCase "true and false" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.false) @=? retFalse
    , testCase "true and true" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.true) @=? retTrue
    , testCase "true or false" $ runMachineBool (B.toNumber $ B.true `B.apOr` B.false) @=? retTrue
    , testCase "false or false" $ runMachineBool (B.toNumber $ B.false `B.apOr` B.false) @=? retFalse
    ]

    where
    retTrue = retVal 1
    retFalse = retVal 0
