{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Expressionism.GraphReducer where

import           Data.Bifunctor  (second)
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
    | Update Int
    | PushGlobal Name
    | PushInt Int
    | Push Int
    | Pop Int
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
    | GNodeInd Addr
    deriving Eq

instance Show GNode where
    show (GNodeNum i)        = "{{" <> show i <> "}}"
    show (GNodeAp a b)       = "NAp " <> show a <> " " <> show b
    show (GNodeGlobal i ops) = "G." <> show i <> ": " <> show ops
    show (GNodeInd a)        = "# " <> show a


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


setCode :: [GOp] -> Machine -> Machine
setCode ops m = m { machineCode = ops }


-- | Simplistic allocation function that uses the next available address
allocate :: Heap -> GNode -> (Addr, Heap)
allocate (a, h) g = (a, (a+1, Map.insert a g h))


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

    Unwind : ops -> case machineStack m of
        s@(a : as) -> case Map.lookup a . snd $ machineHeap m of
            Just (GNodeNum n) -> Right $ setCode [] m

            Just (GNodeAp a1 _) -> Right $ pushStack a1 m

            Just (GNodeGlobal i ops')
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

    _ -> Left NoInstructions


-- | Compile a supercombinator
compile :: CoreSC -> (Name, Word8, [GOp])
compile (name, args, body) = (name, fromIntegral (length args), code)
    where
    d = length args
    positions = Map.fromList $ zip args [0..]
    code = compileC positions body ++ [Update d, Pop d, Unwind]


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
    { machineCode = [PushGlobal "main", Unwind]
    , machineStack = []
    , machineHeap = h0
    , machineGlobals = g
    }

    where
    (h0, g) = foldl step (emptyHeap, Map.empty) $ ("main", [], st) : defs
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
