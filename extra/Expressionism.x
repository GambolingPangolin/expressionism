{
module Expressionism.Lexer
    ( Token (..)
    , scanTokens
    ) where

import Expressionism (Name (Name))
import Data.Text (pack)

}

%wrapper "basic"

$digit = [0-9]
$lowercase = [a-z]
$alpha = [a-zA-Z]
$symb  = [\{\}\(\)\;\.\,\=\+\-\*\/\_\^\>\<]

token :-

$white+                         ;
let                             { const Let }
letrec                          { const LetRec }
in                              { const In }
case                            { const Case }
of                              { const Of }
\->                             { const Arrow }
$digit+                         { IntLit . read }
$symb                           { Sym . head }
λ                               { const Lambda }
\\                              { const Lambda }
C                               { const Constr }
$lowercase [$alpha $digit \_ \']*   { Ident . Name . pack }

{

data Token
    = Let
    | LetRec
    | In
    | Case
    | Of
    | Arrow
    | Lambda

    | Sym Char

    | Constr
    | Ident Name

    | IntLit Int
    deriving (Eq, Show)


scanTokens :: String -> [Token]
scanTokens = alexScanTokens

}

-- vim: sw=4 ts=4
