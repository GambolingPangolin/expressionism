{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Expressionism.Compiler where

import           Control.Monad             (join)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT,
                                            modify)
import           Data.Function             (on)
import           Data.Functor              ((<&>))
import           Data.List                 (intercalate, sortBy)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.String               (fromString)
import           Data.Word                 (Word32, Word64, Word8)

import           Expressionism             (CoreExpr, CoreProgram, CoreSC,
                                            Expr (..), Name)
import           Expressionism.Machine     (GOp (..), GraphNode (..),
                                            Machine (..), MachineT, addGlobal,
                                            allocate, blankMachine, boxNode,
                                            emptyHeap, pushCode)


cnct :: (Applicative f, Monoid a) => f a -> f a -> f a
cnct x y = (<>) <$> x <*> y


type Environment = Map Name Int


-- | Compile a supercombinator
compile :: Monad m => CoreSC -> MachineT m ()
compile (name, args, body) =
    compileE positions body >>= \code ->
    let node = GNodeGlobal (fromIntegral d) . Left $ code <> [Update d, Pop d, Unwind] in
    allocate node >>= \addr ->
    addGlobal name addr
    where
    d = length args
    positions = Map.fromList $ zip args [0..]


-- | Strict compilation
compileE :: Monad m => Environment -> CoreExpr -> MachineT m [GOp]
compileE env = \case
    Nmbr n -> pure [PushInt n]

    Let isRec defs body -> compileLet compileE env isRec defs body

    Ident "negate" `Ap` e -> compileE env e `cnct` pure [Neg]

    Ident op `Ap` e1 `Ap` e2
        | Just gop <- lookup op dyadics
        -> compileE env e2 `cnct` compileE (shift 1 env) e1 `cnct` pure [gop]

    e -> compileC env e `cnct` pure [Eval]


-- | Generate 'GOp' instructions from an expression
compileC :: Monad m => Environment -> CoreExpr -> MachineT m [GOp]
compileC env = \case
    Ident x ->
        pure $ maybe [PushGlobal x] (pure . Push) $ Map.lookup x env

    Ap x y ->
        compileC env y `cnct` compileC (shift 1 env) x `cnct` pure [MkAp]

    Nmbr n -> pure [PushInt n]

    Constr t n -> pure [PushData t n]

    Case e alts ->
        compileE env e `cnct`
        traverse (compileAlt env) (sortBy (compare `on` pr1) alts) `cnct`
        pure [Push m, CaseJump, Slide m, Push 1, Unpack, PushN, PushCode]
        where
        m = length alts
        pr1 (x, _, _) = x
        compileAlt env (_, args, expr) =
            compileC (addLocals args $ shift 2 env) expr >>= storeCode
            where
            d = length args
            storeCode =
                fmap PushRef . allocate
                . GNodeGlobal (fromIntegral d)
                . Left
                . (<> [Slide $ d + 2])

    Let isRec defs body -> compileLet compileC env isRec defs body


compileLet :: Monad m
    => (Environment -> CoreExpr -> MachineT m [GOp])
    -> Environment
    -> Bool
    -> [(Name, CoreExpr)]
    -> CoreExpr
    -> MachineT m [GOp]
compileLet compiler env isRec defs body
    | isRec =
        traverse (compileDefRec n env') defs' >>= \defsCode ->
        pure ([Alloc n] <> join defsCode) `cnct`
        compiler env' body `cnct` pure [Slide n]


    | otherwise =
        traverse (compileDef env) defs' >>= \defsCode ->
        pure (join defsCode) `cnct` compiler env' body `cnct` pure [Slide n]

    where
    n = length defs
    defs' = zip [0..] $ snd <$> defs
    env' = addLocals (fst <$> defs) env


compileDef :: Monad m => Environment -> (Int, CoreExpr) -> MachineT m [GOp]
compileDef env (i, def) = compileC (shift i env) def


compileDefRec :: Monad m => Int -> Environment -> (Int, CoreExpr) -> MachineT m [GOp]
compileDefRec n env (i, def) = compileC env def `cnct` pure [ Update (n-1-i) ]


shift :: Int -> Environment -> Environment
shift i = fmap (+i)


addLocals :: [Name] -> Environment -> Environment
addLocals args env = envLocal <> shift n env
    where
    n = length args
    envLocal = Map.fromList $ zip [1..] args <&> \(i, name) -> (name, n - i)


-- | Prepare the starting state of the machine
initMachine :: Monad m => CoreExpr -> CoreProgram -> m Machine
initMachine st defs
    = execStateT init blankMachine

    where
    init =
        traverse install primitives >>
        traverse compile (("main", [], st) : defs) >>
        pushCode [PushGlobal "main", Unwind]

    install (name, argN, code) =
        allocate (GNodeGlobal argN (Left code)) >>= addGlobal name


primitives :: [(Name, Word8, [GOp])]
primitives = ds <> [ negateC, printC ]

    where
    negateC = ("negate", 1, [Eval, Neg, Update 1, Pop 1, Unwind])
    printC = ("print", 1, [Eval, Print, Pop 1, Unwind])
    ds = uncurry dyadicPrimitive <$> dyadics


dyadics :: [(Name, GOp)]
dyadics =
    [ ("+", Add), ("-", Sub), ("*", Mul)
    , ("<", IsLT), (">", IsGT), ("==", IsEQ)
    ]


dyadicPrimitive :: Name -> GOp -> (Name, Word8, [GOp])
dyadicPrimitive name op = (name, 2, [Eval, Push 1, Eval, Push 1, op, Update 2, Pop 2, Unwind])
