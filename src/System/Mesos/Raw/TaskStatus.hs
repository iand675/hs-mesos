module System.Mesos.Raw.TaskStatus where
import           System.Mesos.Internal
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.TaskId

type TaskStatusPtr = Ptr TaskStatus

foreign import ccall unsafe "ext/types.h toTaskStatus" c_toTaskStatus
  :: TaskIDPtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> SlaveIDPtr
  -> ExecutorIDPtr
  -> Ptr CDouble
  -> Ptr CBool
  -> IO TaskStatusPtr

foreign import ccall unsafe "ext/types.h fromTaskStatus" c_fromTaskStatus
  :: TaskStatusPtr
  -> Ptr TaskIDPtr
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr SlaveIDPtr
  -> Ptr ExecutorIDPtr
  -> Ptr CBool
  -> Ptr CDouble
  -> Ptr CBool -- ^ taskStatusHealthy is set?
  -> Ptr CBool -- ^ taskStatusHealthy value (if set)
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyTaskStatus" c_destroyTaskStatus
  :: TaskStatusPtr
  -> IO ()

instance CPPValue TaskStatus where

  marshal s = do
    tidP <- cppValue $ taskStatusTaskId s
    sidP <- maybe (return nullPtr) cppValue $ taskStatusSlaveId s
    (tmp, tml) <- maybeCString $ taskStatusMessage s
    (tsd, tsl) <- maybeCString $ taskStatusData s
    eidP <- maybe (return nullPtr) cppValue $ taskStatusExecutorId s
    tsp <- alloc
    tsp' <- maybe (return nullPtr) (\x -> poke tsp (CDouble x) >> return tsp) $ taskStatusTimestamp s
    hp <- allocMaybe $ fmap toCBool $ taskStatusHealthy s
    liftIO $ c_toTaskStatus tidP (fromIntegral $ fromEnum $ taskStatusState s) tmp (fromIntegral tml) tsd (fromIntegral tsl) sidP eidP tsp' hp

  unmarshal s = do
    tidp <- alloc
    sp <- alloc
    mpp <- alloc
    mlp <- alloc
    dpp <- alloc
    dlp <- alloc
    sidp <- alloc
    eidp <- alloc
    tssp <- alloc
    poke tssp 0
    tsp <- alloc
    hp <- alloc
    hsp <- alloc
    poke mpp nullPtr
    poke dpp nullPtr
    poke sidp nullPtr
    poke eidp nullPtr
    liftIO $ c_fromTaskStatus s tidp sp mpp mlp dpp dlp sidp eidp tssp tsp hsp hp
    tid <- unmarshal =<< peek tidp
    state <- (toEnum . fromIntegral) <$> peek sp
    msg <- peekMaybeBS mpp mlp
    dat <- peekMaybeBS dpp dlp
    sid <- peekMaybeCPP sidp
    eid <- peekMaybeCPP eidp
    h <- peekMaybePrim hp hsp
    ts <- fmap (\(CDouble d) -> d) <$> peekMaybePrim tsp tssp
    return $ TaskStatus tid state msg dat sid eid ts (fmap fromCBool h)

  destroy = c_destroyTaskStatus
