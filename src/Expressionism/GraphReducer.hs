{-# LANGUAGE LambdaCase #-}

module Expressionism.GraphReducer where

import           Control.Monad              (replicateM, void)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.State  (evalStateT, get, modify, put)
import           Data.Bifunctor             (second)
import           Data.Bool                  (bool)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Word                  (Word32, Word64, Word8)

import           Expressionism              (CoreExpr, CoreProgram, CoreSC,
                                             Expr (..), Name)
import           Expressionism.Machine      (Addr, GNode, GOp, GraphNode (..),
                                             GraphOp (..), Heap, Machine (..),
                                             MachineError (..), MachineT,
                                             boxNode, output, popInstruction,
                                             pushAddr, pushCode, setCode,
                                             updateHeap)


type ExecT m = ExceptT MachineError (MachineT m)


slide :: Monad m => Int -> ExecT m ()
slide n =
    lift get >>= inner
    where
    inner m@Machine{ machineStack = s : ss } =
        lift . modify $ \m -> m { machineStack = s : drop n ss }
    inner _ = throwE StackUnderflow


alloc :: Monad m => Int -> ExecT m [Addr]
alloc n = lift $ replicateM n (boxNode GNodeEmpty)


unwind :: Monad m => ExecT m ()
unwind = lift get >>= \m ->
    case machineStack m of
        s@(a : as) -> case Map.lookup a . snd $ machineHeap m of
            Just (GNodeNum _) -> return ()

            Just (GNodeData _ _) -> return ()

            Just (GNodeAp a1 _) -> lift $ pushAddr a1 >> pushCode [Unwind]

            Just (GNodeFun i code) -> appHead i (Left code)

            Just (GNodeConstr i t) -> appHead i (Right t)

            Just (GNodeInd a') ->
                lift $
                    pushCode [Unwind] >>
                    modify (\m -> m { machineStack = a' : as })

            where
            appHead i body
                | length as < fromIntegral i
                = throwE MissingArguments

                | otherwise
                = lift . update =<< traverse resolve top

                where
                code = either id (constrCode i) body
                constrCode n t = [Pack t n, Update 0, Unwind]

                top = take (fromIntegral i) as
                bot = drop (fromIntegral i) s

                update newTop =
                    pushCode code >>
                    modify (\m -> m { machineStack = newTop <> bot })

                resolve a = case Map.lookup a . snd $ machineHeap m of
                    Just (GNodeAp _ a') -> pure a'
                    _                   -> throwE BadPointer


        _ -> throwE StackUnderflow


update :: Monad m => Int -> ExecT m ()
update n = lift $ get >>= \m ->
    let an = as !! n
        a : as = machineStack m
    in
    updateHeap an (GNodeInd a) >>
    modify (\m -> m { machineStack = as })


pack :: Monad m => Word64 -> Word8 -> ExecT m Addr
pack t n = lift get >>= inner
    where
    inner m
        | length args == fromIntegral n
        = lift $
            modify (\m -> m { machineStack = s }) >>
            boxNode (GNodeData t args)

        | otherwise = throwE MissingArguments

        where
        (args, s) = splitAt (fromIntegral n) $ machineStack m


caseJump :: Monad m => ExecT m ()
caseJump = lift get >>= inner
    where
    inner m
        | a : as <- machineStack m
        , Just (GNodeData t _) <- viewHeap m a
        = lift $
            pushCode [Push (fromIntegral t)] >>
            modify (\m -> m { machineStack = as })

        | otherwise = throwE BadPointer


unpack :: Monad m => ExecT m ()
unpack = lift get >>= inner
    where
    inner m
        | a : as <- machineStack m
        , Just (GNodeData _ xs) <- viewHeap m a
        = lift $
            modify (\m -> m { machineStack = xs <> as })

        | otherwise
        = throwE BadPointer


pushGlobal :: Monad m => Name -> ExecT m ()
pushGlobal x = lift get >>= inner
    where
    inner m =
        let g = Map.lookup x $ machineGlobals m
        in maybe (throwE MissingGlobal) (lift . pushAddr) g


pushRef :: Monad m => Addr -> ExecT m ()
pushRef = lift . pushAddr


pushCodeOp :: Monad m => ExecT m [GOp]
pushCodeOp = lift get >>= inner
    where
    inner m
        | a : as <- machineStack m
        , Just (GNodeFun _ code) <- viewHeap m a
        = lift $
            pushCode code >>
            modify (\m -> m { machineStack = as }) >>
            return code

        | otherwise
        = throwE BadPointer


pushData :: Monad m => Word64 -> Word8 -> ExecT m Addr
pushData t n = lift $ boxNode (GNodeConstr n t)


pushInt :: Monad m => Int -> ExecT m Addr
pushInt = lift . boxNode . GNodeNum


push :: Monad m => Int -> ExecT m ()
push n = lift $ get >>= inner
    where
    inner m = pushAddr (machineStack m !! n)


pop :: Monad m => Int -> ExecT m ()
pop n =
    lift $ get >>= \m ->
    put $ m { machineStack = drop n (machineStack m) }


pushN :: Monad m => ExecT m ()
pushN = lift get >>= inner
    where
    inner m
        | a : as <- machineStack m
        , Just (GNodeNum n) <- viewHeap m a
        = lift . modify $ \m ->
            m { machineStack = (as !! n) : as }

        | otherwise = throwE BadPointer


mkAp :: Monad m => ExecT m Addr
mkAp = lift get >>= inner
    where
    inner m =
        case machineStack m of
            a1 : a2 : ss ->
                lift $
                    modify (\m -> m { machineStack = ss }) >>
                    boxNode (GNodeAp a1 a2)

            _ -> throwE StackUnderflow


neg :: Monad m => ExecT m Addr
neg = lift get >>= inner
    where
    inner m = case machineStack m of
        a : as
            | Just (GNodeNum x) <- Map.lookup a . snd . machineHeap $ m
            -> lift $ boxNode (GNodeNum $ negate x)

            | otherwise -> throwE BadPointer

        _ -> throwE StackUnderflow



printOp :: Monad m => ExecT m ()
printOp = lift get >>= inner
    where
    inner m
        | a : _ <- machineStack m
        = maybe (throwE BadPointer) (lift . output) $ chaseRefs (machineHeap m) a


toInt :: Integral a => Bool -> a
toInt = bool 0 1


viewHeap :: Machine -> Addr -> Maybe GNode
viewHeap m a = Map.lookup a . snd . machineHeap $ m


checkResult :: Monad m => ExecT m (Maybe GNode)
checkResult = lift get >>= inner
    where

    returnable g@(GNodeNum _)    = Just g
    returnable g@(GNodeData _ _) = Just g
    returnable _                 = Nothing

    inner m
        | [] <- machineCode m
        , a : _ <- machineStack m
        = return $ returnable =<< viewHeap m a

        | not . null $ machineCode m
        = return Nothing

        | otherwise = throwE NoInstructions


machineStep :: Monad m => ExecT m (Maybe GNode)
machineStep = lift popInstruction >>= maybe checkResult (nothing . inner)
    where
    nothing = (>> return Nothing)
    inner = \case

        Slide n -> slide n
        Unwind -> unwind

        Alloc n -> void $ alloc n
        Update n -> update n
        Pack t n -> void $ pack t n
        Unpack -> unpack

        CaseJump -> caseJump

        PushGlobal x -> pushGlobal x
        PushCode -> void pushCodeOp
        PushRef a -> pushRef a
        PushData t n -> void $ pushData t n
        PushInt n -> void $ pushInt n
        Push n -> push n
        Pop n -> pop n
        PushN -> pushN

        MkAp -> void mkAp

        Neg -> void neg
        Add -> dyadic (+)
        Sub -> dyadic (-)
        Mul -> dyadic (*)
        IsLT -> dyadic (\x y -> toInt $ x < y)
        IsGT -> dyadic (\x y -> toInt $ x > y)
        IsEQ -> dyadic (\x y -> toInt $ x == y)

        Print -> printOp


newtype Graph = Graph (GraphNode Graph)
    deriving (Eq, Show)


chaseRefs :: Heap -> Addr -> Maybe Graph
chaseRefs hp@(_, h) a = case Map.lookup a h of
    Just (GNodeNum i)     -> Just (Graph $ GNodeNum i)
    Just (GNodeData i as) -> Graph . GNodeData i <$> traverse (chaseRefs hp) as
    _                     -> Nothing


dyadic :: Monad m => (Int -> Int -> Int) -> ExecT m ()
dyadic op = lift get >>= void . inner
    where
    inner m = case machineStack m of
        a : b : xs
            | Just (GNodeNum x) <- getNum m a
            , Just (GNodeNum y) <- getNum m b
            -> lift $ boxNode (GNodeNum $ x `op` y)

            | otherwise -> throwE BadPointer

        _ -> throwE StackUnderflow

    getNum m = flip Map.lookup . snd $ machineHeap m


runExecT :: Monad m => ExecT m a -> Machine -> m (Either MachineError a)
runExecT ops = evalStateT (runExceptT ops)
