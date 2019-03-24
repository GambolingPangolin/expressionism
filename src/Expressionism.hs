-- |
-- Module: Expressionism
--
-- Library for working with the /Core/ language


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Expressionism where


import           Data.Bool   (bool)
import           Data.String (IsString)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Word   (Word64)


newtype Name = Name { unName :: Text }
    deriving (Eq, Ord, IsString)

instance Show Name where
    show = show . unName


-- | The type of Core expressions parameterized over binders
data Expr a
    = Ident a
    | Nmbr Int
    | Constr Word64 Word64
    | Ap (Expr a) (Expr a)
    | Let Bool [(a, Expr a)] (Expr a)
    | Case (Expr a) [(Word64, [a], Expr a)]
    | Lam [a] (Expr a)
    deriving (Eq, Show)


type CoreExpr = Expr Name


-- | Test if the expression is atomic: a var or a number literal
isAtomic :: Expr a -> Bool
isAtomic (Ident _) = True
isAtomic (Nmbr _)  = True
isAtomic _         = False


type CoreSC = (Name, [Name], Expr Name)
type CoreProgram = [ CoreSC ]


preludeDefs :: CoreProgram
preludeDefs =
    [ ("I", ["x"], Ident "x")
    , ("K", ["x", "y"], Ident "x")
    , ("K1", ["x", "y"], Ident "y")
    , ("S", ["f", "g", "x"], (Ident "f" `Ap` Ident "x") `Ap` (Ident "g" `Ap` Ident "x"))
    , ("compose", ["f", "g", "x"], Ident "f" `Ap` (Ident "g" `Ap` Ident "x"))
    , ("twice", ["f"], (Ident "compose" `Ap` Ident "f") `Ap` Ident "f")
    ]


-- | A simple pretty printer, which currently does not do indentation
pretty :: Expr Name -> Text
pretty (Ident a) = unName a
pretty (Nmbr n) = T.pack $ show n
pretty (Constr t i) = T.pack $ "Pack{" <> show t <> "," <> show i <> "}"
pretty (Ap e1 e2) = "(" <> pretty e1 <> " " <> pretty e2 <> ")"
pretty (Let isRec defs body) =
    bool "let" "letrec" isRec <> " " <> T.intercalate "; " (prettyDef <$> defs) <> "in " <> pretty body
    where
    prettyDef (Name x, y) = x <> " = " <> pretty y
pretty (Case x branches) =
    "case " <> pretty x <> " of " <> T.intercalate "; " (prettyBranch <$> branches)
    where
    prettyBranch (ix, vars, body) =
        "{" <> T.pack (show ix) <> "} " <> T.intercalate " " (unName <$> vars)
        <> " -> " <> pretty body
pretty (Lam vars body) =
    "λ " <> T.intercalate " " (unName <$> vars) <> ". " <> pretty body


printProgram :: CoreProgram -> Text
printProgram = T.intercalate "\n\n" . fmap printDef
    where
    printDef (Name x, vars, body) = x <> " " <> T.intercalate " " (unName <$> vars) <> " = " <> pretty body
