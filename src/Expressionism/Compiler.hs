{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Expressionism.Compiler where

import           Control.Monad             (join, void)
import           Control.Monad.Trans.State (StateT, evalStateT, execStateT, get,
                                            modify, put)
import           Crypto.Hash               (SHA256, hash)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS8
import           Data.Function             (on)
import           Data.Functor              ((<&>))
import           Data.Functor.Identity     (runIdentity)
import           Data.List                 (intercalate, sortBy)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Ord                  (Down (..))
import           Data.String               (fromString)
import           Data.Word                 (Word32, Word64, Word8)

import           Expressionism             (CoreExpr, CoreProgram, CoreSC,
                                            Expr (..), Name)
import           Expressionism.Machine     (GOp, GraphNode (..), GraphOp (..),
                                            Machine (..), MachineT, addGlobal,
                                            allocate, blankMachine, boxNode,
                                            emptyHeap, pushCode, updateHeap,
                                            viewGlobal)



type Environment = Map Name Int
type Compiler m = Environment -> CoreExpr -> MachineT m [GOp]


-- | Compile a supercombinator
compileSC :: Monad m => [Name] -> CoreExpr -> MachineT m [GOp]
compileSC args body =
    compileE positions body <&> \code ->
        code <> [Update d, Pop d, Unwind]

    where
    d = length args
    positions = Map.fromList $ zip args [0..]


-- | Strict compilation.  Reduces the expression down to an int or satuarated data constructor
compileE :: Monad m => Environment -> CoreExpr -> MachineT m [GOp]
compileE env = \case
    Ident "negate" `Ap` e -> compileE env e `cnct` pure [Neg]

    Ident op `Ap` e1 `Ap` e2
        | Just gop <- lookup op dyadics
        -> compileE env e2 `cnct` compileE (shift 1 env) e1 `cnct` pure [gop, Slide 2]

    Nmbr n -> pure [PushInt n]

    Let isRec defs body -> compileLet compileE env isRec defs body

    e -> compileC env e `cnct` pure [Unwind]



-- | Leave a node on top of the stack representing the root node of the graph
-- of the expression
compileC :: Monad m => Environment -> CoreExpr -> MachineT m [GOp]
compileC env = \case
    Nmbr n -> pure [PushInt n]

    Constr t n
        | n > 0 -> pure [PushData t n]
        | otherwise ->
            allocate (GNodeData t []) <&> pure . PushRef

    Ident x ->
        pure $ maybe [PushGlobal (toGlobal x)] (pure . Push) $ Map.lookup x env

    Ap x y ->
        compileC env y `cnct` compileC (shift 1 env) x `cnct` pure [MkAp]

    Case e alts -> compileCase env e alts

    Let isRec defs body -> compileLet compileC env isRec defs body

    Lam args body -> compileLam env args body


compileLet :: Monad m
    => Compiler m
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
    defs' = zip [0..] . reverse $ snd <$> defs
    env' = addLocals (fst <$> defs) env


compileDef :: Monad m => Environment -> (Int, CoreExpr) -> MachineT m [GOp]
compileDef env (i, def) = compileC (shift i env) def


compileDefRec :: Monad m => Int -> Environment -> (Int, CoreExpr) -> MachineT m [GOp]
compileDefRec n env (i, def) = compileC env def `cnct` pure [ Update (n-1-i) ]


compileCase :: Monad m
    => Environment
    -> CoreExpr
    -> [(Word64, [Name], CoreExpr)]
    -> MachineT m [GOp]
compileCase env e alts =
    compileE env e `cnct`
    alternatives `cnct`
    pure ([Push m, CaseJump, Slide m, MkAp] <> closure d)

    where
    m = length alts
    d = Map.size env
    alternatives = traverse (compileAlt env . pr23) (sortBy (compare `on` Down . pr1) alts)

    pr1 (x, _, _) = x
    pr23 (_, x, y) = (x, y)


compileAlt :: Monad m => Environment -> ([Name], CoreExpr) -> MachineT m GOp
compileAlt env (args, expr) =
    compileC (addLocals args env) expr >>= \code ->
    let code' = Unpack : code <> [Update evalArity, Pop evalArity, Unwind] in
    allocate (GNodeFun captureArity code') <&>
    PushRef
    where
    d = length args
    evalArity = d + Map.size env
    captureArity = fromIntegral $ 1 + Map.size env


compileLam :: Monad m => Environment -> [Name] -> CoreExpr -> MachineT m [GOp]
compileLam env args body =
    compileC (addLocalsBefore args env) body >>= \code ->
    let code' = code <> [Update arity, Pop arity, Unwind] in
    allocate (GNodeFun (fromIntegral arity) code') >>=
    addGlobal name >>
    pure ([PushGlobal name] <> closure d)
    where
    d = Map.size env
    name =
        fromString
        . show
        . hash @ByteString @SHA256
        . BS8.pack
        . show
        $ (env, args, body)
    arity = Map.size env + length args


shift :: Int -> Environment -> Environment
shift i = fmap (+i)


addLocals :: [Name] -> Environment -> Environment
addLocals args env = envLocal <> shift n env
    where
    n = length args
    envLocal = Map.fromList $ zip args [0..]


addLocalsBefore :: [Name] -> Environment -> Environment
addLocalsBefore args env = envLocal <> env
    where
    d = Map.size env
    envLocal = Map.fromList $ zip args [d..]


closure :: Int -> [GOp]
closure d = [1..d] >>= \i -> [Push i, Push 1, MkAp, Slide 1]


-- | Prepare the starting state of the machine
initMachine :: CoreExpr -> CoreProgram -> Machine
initMachine st defs
    = runIdentity $ execStateT init blankMachine

    where
    allDefs = ("main", [], st) : defs
    init =
        traverse install primitives >>
        traverse allocateGlobals allDefs >>
        traverse compile allDefs >>
        pushCode [PushGlobal "global.main", Unwind]

    install (name, argN, code) =
        allocate (GNodeFun argN code) >>= addGlobal (toGlobal name)

    allocateGlobals (name, _, _) =
        allocate GNodeEmpty >>=
        addGlobal (toGlobal name)

    compile (name, args, body) =
        let f addr = compileSC args body >>= \code ->
                updateHeap addr (GNodeFun (fromIntegral $ length args) code)
        in
        viewGlobal (toGlobal name) >>=
        maybe (return ()) f


primitives :: [(Name, Word8, [GOp])]
primitives = ds <> [ negateC, printC ]

    where
    negateC = ("negate", 1, [Unwind, Neg, Update 1, Pop 1, Unwind])
    printC = ("print", 1, [Unwind, Print, Pop 1, Unwind])
    ds = uncurry dyadicPrimitive <$> dyadics


dyadics :: [(Name, GOp)]
dyadics =
    [ ("+", Add), ("-", Sub), ("*", Mul)
    , ("<", IsLT), (">", IsGT), ("==", IsEQ)
    ]


dyadicPrimitive :: Name -> GOp -> (Name, Word8, [GOp])
dyadicPrimitive name op = (name, 2, [Unwind, Push 1, Unwind, Push 1, op, Slide 2, Update 2, Pop 2, Unwind])


cnct :: (Applicative f, Monoid a) => f a -> f a -> f a
cnct x y = (<>) <$> x <*> y


toGlobal :: Name -> Name
toGlobal x = "global." <> x
