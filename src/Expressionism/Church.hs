{-# LANGUAGE OverloadedStrings #-}

module Expressionism.Church where

import           Data.String   (fromString)
import           Data.Word     (Word32)
import           Expressionism (CoreSC, Expr (..), Name (..))



church :: Word32 -> CoreSC
church 0 = ("church0", ["f", "x"], Ident "x")
church n = (name, ["f", "x"], Ident "f" `Ap` church')
    where
    name = fromString $ "church" <> show n
    (_, _, church') = church (n-1)


plus :: CoreSC
plus =
    ( "plus"
    , ["n", "m", "f", "x"]
    , Ident "n" `Ap` (Ident "m" `Ap` Ident "f") `Ap` Ident "x"
    )


times :: CoreSC
times =
    ( "times"
    , ["n", "m", "f", "x"]
    , Ident "n" `Ap` Ident "m" `Ap` Ident "f" `Ap` Ident "x"
    )


churchPrelude :: [CoreSC]
churchPrelude = [plus, times] <> (church <$> [0..25])
