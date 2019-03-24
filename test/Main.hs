{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit           (testCase, (@=?))

import           Expressionism              (Expr (..), preludeDefs)
import qualified Expressionism.Bool         as B
import           Expressionism.GraphReducer (execBounded, initMachine, result)


main :: IO ()
main = defaultMain $ testGroup "tests" [preludeTests, boolTests]


runMachine x y = fmap result . execBounded 100 $ initMachine x y

runMachine0 = flip runMachine preludeDefs
runMachineBool = flip runMachine B.preludeBool


preludeTests :: TestTree
preludeTests = testGroup "prelude"
    [ testCase "I should compute the identity" $
        runMachine0 (Ap (Ident "I") (Nmbr 1)) @=? Right (Just 1)

    , testCase "K should project the first arg" $
        runMachine0 (Ap (Ap (Ident "K") (Nmbr 1)) (Nmbr 2)) @=? Right (Just 1)

    , testCase "K1 should project the second arg" $
        runMachine0 (Ap (Ap (Ident "K1") (Nmbr 1)) (Nmbr 2)) @=? Right (Just 2)

    ]


boolTests :: TestTree
boolTests = testGroup "bool"
    [ testCase "true" $ runMachineBool (B.toNumber B.true) @=? trueVal
    , testCase "false" $ runMachineBool (B.toNumber B.false) @=? falseVal
    , testCase "not false" $ runMachineBool (B.toNumber $ B.apNot B.false) @=? trueVal
    , testCase "true and false" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.false) @=? falseVal
    , testCase "true and true" $ runMachineBool (B.toNumber $ B.true `B.apAnd` B.true) @=? trueVal
    , testCase "true or false" $ runMachineBool (B.toNumber $ B.true `B.apOr` B.false) @=? trueVal
    , testCase "false or false" $ runMachineBool (B.toNumber $ B.false `B.apOr` B.false) @=? falseVal
    ]

    where
    trueVal = Right (Just 1)
    falseVal = Right (Just 0)
