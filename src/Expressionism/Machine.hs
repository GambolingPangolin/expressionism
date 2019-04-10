{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Expressionism.Machine where

import           Control.Monad.Trans.State (StateT (..), get, modify, put)
import           Data.Bifunctor            (second)
import           Data.List                 (intercalate)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Word                 (Word32, Word64, Word8)


import           Expressionism             (Name)


-- | Instructions for the G-Machine
data GraphOp a
    = Slide Int
    | Unwind
    | Eval
    | Update Int
    | Alloc Int

    | MkAp

    | PushGlobal Name
    | PushRef a
    | PushCode
    | PushData Word64 Word8
    | PushInt Int
    | Push Int
    | PushN
    | Pop Int

    | Pack Word64 Word8
    -- | Inspect the node corresponding to the address on top of the stack and
    -- pop the stack item described by its tag
    | CaseJump
    | Unpack

    | Neg
    | Add | Sub | Mul
    | IsLT | IsGT | IsEQ

    | Print
    deriving (Eq, Show)


type GOp = GraphOp Addr


newtype Addr = Addr Word32
    deriving (Eq, Ord, Num)

instance Show Addr where
    show (Addr w) = show w


-- | Possible nodes that get allocated on the heap
data GraphNode a
    = GNodeNum Int
    -- ^ concrete number
    | GNodeData Word64 [a]
    -- ^ data structure
    | GNodeFun Word8 [GraphOp a]
    -- ^ function definition
    | GNodeConstr Word8 Word64
    -- ^ unapplied data constructor
    | GNodeEmpty
    -- ^ empty node
    | GNodeInd a
    -- ^ indirection node
    | GNodeAp a a
    -- ^ application node
    deriving Eq


type GNode = GraphNode Addr

instance Show a => Show (GraphNode a) where
    show (GNodeNum i)      = "{{" <> show i <> "}}"
    show (GNodeData i as)  = "Pack{" <> show i <> "; " <> show as <> "}"
    show (GNodeAp a b)     = "NAp " <> show a <> " " <> show b
    show (GNodeFun i ops)  = "G (" <> show i <> "): " <> show ops
    show (GNodeConstr i t) = "C_" <> show t <> " (" <> show i <> ")"
    show (GNodeInd a)      = "# " <> show a
    show GNodeEmpty        = "-()-"


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
    , machineOutput  :: String
    } deriving Eq

instance Show Machine where
    show m = intercalate "\n"
        [ "Code: " <> show (machineCode m)
        , "Stack: " <> show (machineStack m)
        , "Freezer:\n\t" <> showFreezer (machineFreezer m)
        , "Heap: \t" <> showHeap (machineHeap m)
        , "Globals: " <> show (machineGlobals m)
        , "Output: " <> show (machineOutput m)
        ]

showHeap :: Heap -> String
showHeap (_, h) = intercalate "\n\t" $ showItem <$> Map.toList h
    where
    showItem (a, n) = show a <> " ~> " <> show n


showFreezer :: [([GOp], Stack)] -> String
showFreezer = intercalate "\n\n\t" . fmap showItem
    where
    showItem (c, s) = show c <> "\n\t" <> show s


blankMachine :: Machine
blankMachine =
    Machine
    { machineCode = []
    , machineStack = []
    , machineFreezer = []
    , machineHeap = emptyHeap
    , machineGlobals = Map.empty
    , machineOutput = ""
    }


type MachineT m = StateT Machine m


output :: (Show a, Monad m)
    => a
    -> MachineT m ()
output x = modify $ \m -> m { machineOutput = reverse (show x) <> "\n" <> machineOutput m }


pushAddr :: Monad m => Addr -> MachineT m ()
pushAddr addr = modify $ \m -> m { machineStack = addr : machineStack m }


setCode :: Monad m => [GOp] -> MachineT m ()
setCode ops = modify $ \m -> m { machineCode = ops }


pushCode :: Monad m => [GOp] -> MachineT m ()
pushCode code = modify $ \m -> m { machineCode = code <> machineCode m }


-- | Simplistic allocation function that uses the next available address
allocate :: Monad m => GNode -> MachineT m Addr
allocate g =
    get >>= \m ->
    let (a, h) = machineHeap m in
    modify (\m -> m { machineHeap = (a+1, Map.insert a g h) }) >>
    return a


boxNode :: Monad m => GNode -> MachineT m Addr
boxNode g =
    allocate g >>= \addr ->
    pushAddr addr >>
    return addr


updateHeap :: Monad m => Addr -> GNode -> MachineT m ()
updateHeap a n =
    let f = second (Map.insert a n) in
    modify $ \m -> m { machineHeap = f (machineHeap m) }


addGlobal :: Monad m => Name -> Addr -> MachineT m ()
addGlobal name addr = modify $ \m ->
    m { machineGlobals = Map.insert name addr $ machineGlobals m }


popInstruction :: Monad m => MachineT m (Maybe GOp)
popInstruction = get >>= inner
    where
    inner m@Machine { machineCode = i : is } = put (m { machineCode = is }) >> return (Just i)
    inner _ = return Nothing


emptyHeap :: Heap
emptyHeap = (0, Map.empty)


data MachineError
    = MissingGlobal
    | MissingArguments
    | NoInstructions
    | StackUnderflow
    | BadPointer
    deriving (Eq, Show)
