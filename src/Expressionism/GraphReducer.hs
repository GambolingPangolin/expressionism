{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Expressionism.GraphReducer where

import           Control.Monad   (join)
import           Data.Bifunctor  (second)
import           Data.Bool       (bool)
import           Data.Functor    ((<&>))
import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word       (Word32, Word8)

import           Expressionism   (CoreExpr, CoreProgram, CoreSC, Expr (..),
                                  Name)


-- | Instructions for the G-Machine (Mark I)
data GOp
    = Slide Int
    | Unwind
    | Eval
    | Update Int
    | Alloc Int

    | MkAp

    | PushGlobal Name
    | PushInt Int
    | Push Int
    | Pop Int

    | Branch

    | Neg
    | Add | Sub | Mul
    | IsLT | IsGT | IsEQ
    deriving (Eq, Show)


newtype Addr = Addr Word32
    deriving (Eq, Ord, Num)

instance Show Addr where
    show (Addr w) = show w


-- | Possible nodes that get allocated on the heap
data GNode
    = GNodeNum Int
    | GNodeAp Addr Addr
    | GNodeGlobal Word8 [GOp]
    | GNodeInd Addr
    | GNodeEmpty
    deriving Eq

instance Show GNode where
    show (GNodeNum i)        = "{{" <> show i <> "}}"
    show (GNodeAp a b)       = "NAp " <> show a <> " " <> show b
    show (GNodeGlobal i ops) = "G." <> show i <> ": " <> show ops
    show (GNodeInd a)        = "# " <> show a
    show GNodeEmpty          = "-()-"


type Stack = [Addr]
type Heap = (Addr, Map Addr GNode)


-- | Machine state
data Machine
    = Machine
    { machineCode    :: [GOp]
    , machineStack   :: Stack
    , machineFreezer :: [([GOp], Stack)]
    , machineHeap    :: Heap
    , machineGlobals :: Map Name Addr
    } deriving Eq

instance Show Machine where
    show m = intercalate "\n"
        [ "Code: " <> show (machineCode m)
        , "Stack: " <> show (machineStack m)
        , "Freezer:\n\t" <> showFreezer (machineFreezer m)
        , "Heap: \t" <> showHeap (machineHeap m)
        , "Globals: " <> show (machineGlobals m)
        ]

showHeap :: Heap -> String
showHeap (_, h) = intercalate "\n\t" $ showItem <$> Map.toList h
    where
    showItem (a, n) = show a <> " ~> " <> show n


showFreezer :: [([GOp], Stack)] -> String
showFreezer = intercalate "\n\n\t" . fmap showItem
    where
    showItem (c, s) = show c <> "\n\t" <> show s

pushStack :: Addr -> Machine -> Machine
pushStack addr m = m { machineStack = addr : machineStack m }


setCode :: [GOp] -> Machine -> Machine
setCode ops m = m { machineCode = ops }


-- | Simplistic allocation function that uses the next available address
allocate :: Heap -> GNode -> (Addr, Heap)
allocate (a, h) g = (a, (a+1, Map.insert a g h))


boxInteger :: Int -> Machine -> Machine
boxInteger n m = m { machineHeap = h, machineStack = a : machineStack m }
    where
    (a, h) = allocate (machineHeap m) (GNodeNum n)


updateHeap :: Addr -> GNode -> Heap -> Heap
updateHeap a n = second (Map.insert a n)


emptyHeap :: Heap
emptyHeap = (0, Map.empty)


data MachineError
    = MissingGlobal
    | MissingArguments
    | NoInstructions
    | StackUnderflow
    | BadPointer
    deriving (Eq, Show)


machineStep :: Machine -> Either MachineError Machine
machineStep m = case machineCode m of
    Slide n : ops ->
        case machineStack m of
            s : ss -> Right . setCode ops $ m { machineStack = s : drop n ss }
            _      -> Left StackUnderflow

    Alloc n : ops ->
        let allocEmpty m
                | (a, h) <- allocate (machineHeap m) GNodeEmpty
                =  pushStack a $ m { machineHeap = h }
        in Right . setCode ops $  iterate allocEmpty m !! n

    Unwind : ops -> case machineStack m of
        s@(a : as) -> case Map.lookup a . snd $ machineHeap m of
            Just (GNodeNum n) -> case machineFreezer m of
                (ops', s') : ss -> Right $ m { machineCode = ops'
                                             , machineStack = a : s'
                                             , machineFreezer = ss
                                             }
                _ -> Right $ setCode [] m

            Just (GNodeAp a1 _) -> Right $ pushStack a1 m

            Just (GNodeGlobal i ops')
                | length as < fromIntegral i
                , not (null as)
                , (ops', s') : f <- machineFreezer m
                -> Right . setCode (ops' <> ops) $ m { machineStack = last as : s'
                                                     , machineFreezer = f }

                | length as < fromIntegral i    -> Left MissingArguments

                | otherwise                     -> update <$> traverse resolve top

                where
                top = take (fromIntegral i) as
                bot = drop (fromIntegral i) s
                update newTop = setCode (ops' <> ops) $ m { machineStack = newTop <> bot }
                resolve a = case Map.lookup a . snd $ machineHeap m of
                    Just (GNodeAp _ a') -> Right a'
                    _                   -> Left BadPointer

            Just (GNodeInd a') -> Right $ m { machineStack = a' : as }

        _ -> Left StackUnderflow

    Eval : ops
        | a : as <- machineStack m
        -> Right $ m { machineCode = [Unwind]
                     , machineStack = [a]
                     , machineFreezer = (ops, as) : machineFreezer m
                     }

        | otherwise -> Left StackUnderflow

    Branch : ops ->
        case machineStack m of
            a : as
                | Just (GNodeNum 1) <- viewHeap a
                -> Right . setCode (Push 1 : ops) $ m { machineStack = as }

                | Just (GNodeNum 0) <- viewHeap a
                -> Right . setCode (Push 2 : ops) $ m { machineStack = as }

                | otherwise -> Left BadPointer

            _ -> Left StackUnderflow

    Update n : ops ->
        let an = as !! n
            a : as = machineStack m
            h = updateHeap an (GNodeInd a) $ machineHeap m
        in Right . setCode ops $ m { machineStack = as, machineHeap = h }

    PushGlobal x : ops ->
        let g = Map.lookup x $ machineGlobals m
            update a = pushStack a $ setCode ops m
        in maybe (Left MissingGlobal) (Right . update) g

    PushInt x : ops    ->
        let (a, h) = allocate (machineHeap m) $ GNodeNum x in
        Right . pushStack a . setCode ops $ m { machineHeap = h }

    Push n : ops ->
        Right . pushStack (machineStack m !! n) $ setCode ops m

    Pop n : ops ->
        Right $ m { machineStack = drop n (machineStack m), machineCode = ops }

    MkAp : ops ->
        case machineStack m of
            a1 : a2 : ss ->
                let (a, h) = allocate (machineHeap m) $ GNodeAp a1 a2 in
                Right . setCode ops $ m { machineStack = a : ss, machineHeap = h }

            _ -> Left StackUnderflow

    Neg : ops -> case machineStack m of
        a : as
            | Just (GNodeNum x) <- Map.lookup a . snd . machineHeap $ m
            -> Right . setCode ops . boxInteger (negate x) $ m

            | otherwise -> Left BadPointer

        _ -> Left StackUnderflow

    Add : _ -> dyadic (+) m
    Sub : _ -> dyadic (-) m
    Mul : _ -> dyadic (*) m
    IsLT : _ -> dyadic (\x y -> toInt $ x < y) m
    IsGT : _ -> dyadic (\x y -> toInt $ x > y) m
    IsEQ : _ -> dyadic (\x y -> toInt $ x == y) m

    _ -> Left NoInstructions

    where
    toInt = bool 0 1
    viewHeap a = Map.lookup a . snd . machineHeap $ m


dyadic :: (Int -> Int -> Int) -> Machine -> Either MachineError Machine
dyadic op m = case machineStack m of
    a : b : xs
        | Just (GNodeNum x) <- get a
        , Just (GNodeNum y) <- get b
        -> Right
            . setCode (drop 1 (machineCode m))
            . boxInteger (x `op` y) $ m

        | otherwise -> Left BadPointer

    _ -> Left StackUnderflow

    where

    get = flip Map.lookup . snd $ machineHeap m


-- | Compile a supercombinator
compile :: CoreSC -> (Name, Word8, [GOp])
compile (name, args, body) = (name, fromIntegral (length args), code)
    where
    d = length args
    positions = Map.fromList $ zip args [0..]
    code = compileE positions body ++ [Update d, Pop d, Unwind]


-- | Strict compilation
compileE :: Map Name Int -> CoreExpr -> [GOp]
compileE env = \case
    Nmbr n -> [PushInt n]
    Let isRec defs body
        | isRec ->
            join $ [[Alloc n]] <>
                   (compileDefRec n env' <$> defs') <>
                   [compileE env' body <> [Slide n]]

        | otherwise ->
            join $ (compileDef env <$> defs') <> [compileE env' body <> [Slide n]]

        where
        n = length defs
        defs' = zip [0..] defs
        env' = addLocals n defs env

    Ident "negate" `Ap` e -> compileE env e <> [Neg]

    Ident op `Ap` e1 `Ap` e2
        | Just gop <- lookup op dyadics
        -> compileE env e2 <> compileE (shift 1 env) e1 <> [gop]


    e -> compileC env e <> [Eval]


-- | Generate 'GOp' instructions from an expression
compileC :: Map Name Int -> CoreExpr -> [GOp]
compileC env = \case
    Ident x -> maybe [PushGlobal x] (pure . Push) $ Map.lookup x env
    Ap x y -> compileC env y ++ compileC (shift 1 env) x ++ [MkAp]
    Nmbr n -> [PushInt n]

    Let isRec defs body
        | isRec ->
            join $  [[Alloc n]] <>
                    (compileDefRec n env' <$> defs') <>
                    [compileC env' body <> [Slide n]]

        | otherwise ->
            join $ (compileDef env <$> defs') <> [compileC env' body <> [Slide n]]

        where
        n = length defs
        defs' = zip [0..] defs
        env' = addLocals n defs env


compileDef :: Map Name Int -> (Int, (a, CoreExpr)) -> [GOp]
compileDef env (i, (_, def)) = compileC (shift i env) def


compileDefRec :: Int -> Map Name Int -> (Int, (a, CoreExpr)) -> [GOp]
compileDefRec n env (i, (_, def)) = compileC env def <> [ Update (n-1-i) ]


shift :: Int -> Map Name Int -> Map Name Int
shift i = fmap (+i)


addLocals :: Int -> [(Name, CoreExpr)] -> Map Name Int -> Map Name Int
addLocals n defs env = envLocal <> shift n env
    where
    envLocal = Map.fromList $ zip [1..] defs <&> \(i, (name, _)) -> (name, n - i)


-- | Prepare the starting state of the machine
initMachine :: CoreExpr -> CoreProgram -> Machine
initMachine st defs
    = Machine
    { machineCode = [PushGlobal "main", Unwind]
    , machineStack = []
    , machineFreezer = []
    , machineHeap = h0
    , machineGlobals = g0
    }

    where
    compiled = compile <$> (("main", [], st) : defs)
    (h0, g0) = foldl step (emptyHeap, Map.empty) $ compiled <> primitives
    step (h, g) (name, i, code) =
        let (a, h') = allocate h (GNodeGlobal i code)
        in (h', Map.insert name a g)


primitives :: [(Name, Word8, [GOp])]
primitives = ds <> [ neg, ifCombinator ]

    where
    neg = ("negate", 1, [Eval, Neg, Update 1, Pop 1, Unwind])
    ifCombinator = ("if", 3, [Push 0, Eval, Branch, Update 3, Pop 3, Unwind])
    ds = uncurry dyadicPrimitive <$> dyadics


dyadics :: [(Name, GOp)]
dyadics =
    [ ("+", Add), ("-", Sub), ("*", Mul)
    , ("<", IsLT), (">", IsGT), ("==", IsEQ)
    ]


dyadicPrimitive :: Name -> GOp -> (Name, Word8, [GOp])
dyadicPrimitive name op = (name, 2, [Eval, Push 1, Eval, Push 1, op, Update 2, Pop 2, Unwind])


-- | Run a computation for up to the given number of steps
execBounded :: Word32 -> Machine -> Either MachineError Machine
execBounded n m = case machineStep m of
    Right m'    | n > 0 -> execBounded (n-1) m'
                | otherwise -> Right m'
    Left NoInstructions -> Right m
    x -> x


-- | Extract the result from the machine if it
--   1. has no remaining instructions and
--   2. has an int on top of the stack
result :: Machine -> Maybe Int
result m
    | null (machineCode m)
    = case machineStack m of
        a : _ -> case Map.lookup a . snd $ machineHeap m of
            Just (GNodeNum n) -> Just n
            _                 -> Nothing
        _ -> Nothing

    | otherwise
    = Nothing
