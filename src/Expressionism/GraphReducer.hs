{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Expressionism.GraphReducer where

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
    | PushGlobal Name
    | PushInt Int
    | Push Int
    | MkAp
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
    deriving Eq

instance Show GNode where
    show (GNodeNum i)        = "{{" <> show i <> "}}"
    show (GNodeAp a b)       = "NAp " <> show a <> " " <> show b
    show (GNodeGlobal i ops) = "G." <> show i <> ": " <> show ops


type Stack = [Addr]
type Heap = (Addr, Map Addr GNode)


-- | Machine state
data Machine
    = Machine
    { machineCode    :: [GOp]
    , machineStack   :: Stack
    , machineHeap    :: Heap
    , machineGlobals :: Map Name Addr
    } deriving Eq

instance Show Machine where
    show m = intercalate "\n"
        [ "Code: " <> show (machineCode m)
        , "Stack: " <> show (machineStack m)
        , "Heap: \t" <> showHeap (machineHeap m)
        , "Globals: " <> show (machineGlobals m)
        ]

showHeap :: Heap -> String
showHeap (_, h) = intercalate "\n\t" $ showItem <$> Map.toList h
    where
    showItem (a, n) = show a <> " ~> " <> show n


pushStack :: Addr -> Machine -> Machine
pushStack addr m = m { machineStack = addr : machineStack m }


putOps :: [GOp] -> Machine -> Machine
putOps ops m = m { machineCode = ops }

-- | Simplistic allocation function that uses the next available address
allocate :: Heap -> GNode -> (Addr, Heap)
allocate (a, h) g = (a, (a+1, Map.insert a g h))


emptyHeap :: Heap
emptyHeap = (0, Map.empty)


data MachineError
    = MissingGlobal
    | MissingArguments
    | NoInstructions
    | StackUnderflow
    deriving (Eq, Show)


machineStep :: Machine -> Either MachineError Machine
machineStep m = case machineCode m of
    Slide n : ops ->
        case machineStack m of
            s : ss -> Right . putOps ops $ m { machineStack = s : drop n ss }
            _      -> Left StackUnderflow

    Unwind : ops -> case machineStack m of
        s@(a : as) -> case Map.lookup a . snd $ machineHeap m of
            Just (GNodeNum n)         -> Right $ putOps [] m
            Just (GNodeAp a1 _)       -> Right $ pushStack a1 m
            Just (GNodeGlobal i ops')
                | length as < fromIntegral i -> Left MissingArguments
                | otherwise -> Right $ putOps (ops' ++ ops) m

        _ -> Left StackUnderflow

    PushGlobal x : ops ->
        let g = Map.lookup x $ machineGlobals m
            update a = pushStack a $ putOps ops m
        in maybe (Left MissingGlobal) (Right . update) g

    PushInt x : ops    ->
        let (a, h) = allocate (machineHeap m) $ GNodeNum x in
        Right . pushStack a . putOps ops $ m { machineHeap = h }

    Push n : ops ->
        let Just (GNodeAp _ a) = Map.lookup (ss !! n) . snd $ machineHeap m
            _ : ss = machineStack m
        in Right . pushStack a $ putOps ops m

    MkAp : ops ->
        case machineStack m of
            a1 : a2 : ss ->
                let (a, h) = allocate (machineHeap m) $ GNodeAp a1 a2 in
                Right . putOps ops $ m { machineStack = a : ss, machineHeap = h }

            _ -> Left StackUnderflow

    _ -> Left NoInstructions


-- | Compile a supercombinator
compile :: CoreSC -> (Name, Word8, [GOp])
compile (name, args, body) = (name, fromIntegral (length args), code)
    where
    positions = Map.fromList $ zip args [0..]
    code = compileC positions body ++ [Slide (length args + 1), Unwind]


-- | Generate 'GOp' instructions from an expression
compileC :: Map Name Int -> CoreExpr -> [GOp]
compileC env = \case
    Ident x -> maybe [PushGlobal x] (pure . Push) $ Map.lookup x env
    Ap x y -> compileC env y ++ compileC (shift env) x ++ [MkAp]
    Nmbr n -> [PushInt n]

    where
    shift = fmap (+1)


-- | Prepare the starting state of the machine
initMachine :: CoreExpr -> CoreProgram -> Machine
initMachine st defs
    = Machine
    { machineCode = initialCode
    , machineStack = []
    , machineHeap = h0
    , machineGlobals = g
    }

    where
    (_, _, initialCode) = compile ("main", [], st)
    (h0, g) = foldl step (emptyHeap, Map.empty) defs
    step (h, g) sc =
        let (name, i, code) = compile sc
            (a, h') = allocate h (GNodeGlobal i code)
        in (h', Map.insert name a g)


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
