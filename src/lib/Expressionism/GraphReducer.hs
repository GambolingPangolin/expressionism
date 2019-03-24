{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expressionism.GraphReducer where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word       (Word32, Word8)
import           Expressionism   (Expr (..), Name)


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
    deriving (Eq, Ord, Num, Show)


-- | Possible nodes that get allocated on the heap
data GNode
    = GNodeNum Int
    | GNodeAp Addr Addr
    | GNodeGlobal Word8 [GOp]
    deriving (Eq, Show)


type Stack = [Addr]
type Heap = (Addr, Map Addr GNode)


-- | Machine state
data Machine
    = Machine
    { machineCode    :: [GOp]
    , machineStack   :: Stack
    , machineHeap    :: Heap
    , machineGlobals :: Map Name Addr
    } deriving (Eq, Show)


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


step :: Machine -> Either MachineError Machine
step m = case machineCode m of
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
                Right . pushStack a . putOps ops $ m { machineHeap = h }

            _ -> Left StackUnderflow

    _ -> Left NoInstructions
