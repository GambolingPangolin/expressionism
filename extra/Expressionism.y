{
{-# LANGUAGE FlexibleContexts #-}
module Expressionism.Parser (parseExpression) where

import           Data.Word (Word64)
import           Data.Text (pack)

import           Expressionism (Expr (..), CoreExpr, Name (Name), CoreSC)
import           Expressionism.Lexer (Token)
import qualified Expressionism.Lexer as L

}

%name parseExpression
%tokentype { Token }
%error { parseError }

%token
    let         { L.Let }
    letrec      { L.LetRec }
    in          { L.In  }
    case        { L.Case }
    of          { L.Of }
    var         { L.Ident $$ }
    int         { L.IntLit $$ }
    cstr        { L.Constr }
    '->'        { L.Arrow }
    '\\'        { L.Lambda }
    '('         { L.Sym '(' }
    ')'         { L.Sym ')' }
    ';'         { L.Sym ';' }
    '.'         { L.Sym '.' }
    '='         { L.Sym '=' }
    '_'         { L.Sym '_' }
    '^'         { L.Sym '^' }
    '+'         { L.Sym '+' }
    '-'         { L.Sym '-' }
    '*'         { L.Sym '*' }
    '/'         { L.Sym '/' }
    '>'         { L.Sym '>' }
    '<'         { L.Sym '<' }
    '{'         { L.Sym '{' }
    '}'         { L.Sym '}' }

%%

Program     :: { [CoreSC] }
            : SC                                    { [$1] }
            | Program SC                            { $2 : $1 }


SC          :: { CoreSC }
            : var ArgList '=' '{' Exp '}'           { ($1, $2, $5) }


Exp         :: { CoreExpr }
            : Factor                                { $1 }
            | Case                                  { $1 }
            | Let                                   { $1 }
            | LetRec                                { $1 }
            | Lam                                   { $1 }
            | Ap                                    { $1 }


Case        :: { CoreExpr }
            : case Exp of BranchList                { Case $2 $4 }


BranchList  :: { [(Word64, [Name], CoreExpr)] }
            : BranchList_                           { reverse $1 }


BranchList_ :: { [(Word64, [Name], CoreExpr)] }
            : Branch                                { [$1] }
            | BranchList_ ';' Branch                { $3 : $1 }


Branch      :: { (Word64, [Name], CoreExpr) }
            : cstr '_' int ArgList '->' Exp         { (fromIntegral $3, $4, $6) }


Let         :: { CoreExpr }
            : let DefList in Exp                    { Let False $2 $4 }


LetRec      :: { CoreExpr }
            : letrec DefList in Exp                 { Let True $2 $4 }


DefList     :: { [(Name, CoreExpr)] }
            : DefList_                              { reverse $1 }


DefList_    :: { [(Name, CoreExpr)] }
            : Def                                   { [$1] }
            | DefList_ ';' Def                      { $3 : $1 }


Def         :: { (Name, CoreExpr) }
            : var '=' Exp                           { ($1, $3) }


Lam         :: { CoreExpr }
            : '\\' ArgList '.' Exp                  { Lam $2 $4 }


ArgList     :: { [Name] }
            : ArgList_                              { reverse $1 }


ArgList_    :: { [Name] }
            : {- empty -}                           { [] }
            | ArgList_ var                          { $2 : $1 }


Ap          :: { CoreExpr }
            : var Factor                            { Ap (Ident $1) $2 }
            | BuiltIn Factor                        { Ap $1 $2 }
            | Constr Factor                         { Ap $1 $2 }
            | '(' Exp ')' Factor                    { Ap $2 $4 }
            | Ap Factor                             { Ap $1 $2 }


Factor      :: { CoreExpr }
            : int                                   { Nmbr $1 }
            | var                                   { Ident $1 }
            | Constr                                { $1 }
            | '(' Exp ')'                           { $2 }


Constr      :: { CoreExpr }
            : cstr '_' int '^' int                  { Constr (fromIntegral $3) (fromIntegral $5) }


BuiltIn     :: { CoreExpr }
            : '+'                                   { toIdent "+" }
            | '-'                                   { toIdent "-" }
            | '*'                                   { toIdent "*" }
            | '/'                                   { toIdent "/" }
            | '>'                                   { toIdent ">" }
            | '<'                                   { toIdent "<" }
            | '=' '='                               { toIdent "==" }

{

toIdent :: String -> CoreExpr
toIdent = Ident . Name . pack

parseError :: [Token] -> a
parseError _ = error "parsing error"

}
