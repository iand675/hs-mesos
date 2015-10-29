module System.Mesos.Raw.TaskStatus where
import           System.Mesos.Internal
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.TaskId

type TaskStatusPtr = Ptr TaskStatus

foreign import ccall unsafe "ext/types.h toTaskStatus" c_toTaskStatus
  :: TaskIDPtr -- ^ task id
  -> CInt -- ^ state
  -> Ptr CChar -- ^ message
  -> CInt -- ^ message length
  -> Ptr CInt -- ^ task status source
  -> Ptr CInt -- ^ task status reason
  -> Ptr CChar -- ^ data
  -> CInt -- ^ data length
  -> SlaveIDPtr -- ^ slave id
  -> ExecutorIDPtr -- ^ executor id
  -> Ptr CDouble -- timestamp
  -> Ptr CChar -- ^ uuid
  -> CInt -- ^ uuid length
  -> Ptr CBool -- healthcheck
  -> IO TaskStatusPtr

foreign import ccall unsafe "ext/types.h fromTaskStatus" c_fromTaskStatus
  :: TaskStatusPtr -- ^ task status
  -> Ptr TaskIDPtr -- ^ task id
  -> Ptr CInt -- ^ state
  -> Ptr (Ptr CChar) -- ^ message
  -> Ptr CInt -- ^ message length
  -> Ptr CBool -- ^ source set?
  -> Ptr CInt -- ^ source
  -> Ptr CBool -- ^ reason set?
  -> Ptr CInt -- ^ reason
  -> Ptr (Ptr CChar) -- ^ data
  -> Ptr CInt -- ^ data length
  -> Ptr SlaveIDPtr -- ^ slave id
  -> Ptr ExecutorIDPtr -- ^ executor id
  -> Ptr CBool -- ^ timestampSet
  -> Ptr CDouble -- ^ timestamp
  -> Ptr (Ptr CChar) -- ^ uuid
  -> Ptr CInt -- ^ uuid length
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
    sourcep <- alloc
    sourcep' <- maybe (return nullPtr) (\x -> poke sourcep (
                                            CInt (fromIntegral (fromEnum x)))
                                              >> return sourcep) $
                taskStatusSource s
    reasonp <- alloc
    reasonp' <- maybe (return nullPtr) (\x -> poke reasonp (
                                            CInt (fromIntegral (fromEnum x)))
                                              >> return reasonp) $
                taskStatusReason s
    (tsd, tsl) <- maybeCString $ taskStatusData_ s
    eidP <- maybe (return nullPtr) cppValue $ taskStatusExecutorId s
    tsp <- alloc
    tsp' <- maybe (return nullPtr) (\x -> poke tsp (CDouble x) >> return tsp) $ taskStatusTimestamp s
    (uuidp, uuidl) <- maybeCString $ taskStatusUUID s
    hp <- allocMaybe $ fmap toCBool $ taskStatusHealthy s
    liftIO $ c_toTaskStatus tidP (fromIntegral $ fromEnum $ taskStatusState s)
               tmp (fromIntegral tml) sourcep' reasonp' tsd (fromIntegral tsl) sidP
               eidP tsp' uuidp (fromIntegral uuidl) hp

  unmarshal s = do
    tidp <- alloc
    sp <- alloc
    mpp <- alloc
    mlp <- alloc
    sourcesp <- alloc
    poke sourcesp 0
    sourcep <- alloc
    reasonsp <- alloc
    poke reasonsp 0
    reasonp <- alloc
    dpp <- alloc
    dlp <- alloc
    sidp <- alloc
    eidp <- alloc
    tssp <- alloc
    poke tssp 0
    tsp <- alloc
    uuidpp <- alloc
    uuidlp <- alloc
    hp <- alloc
    hsp <- alloc
    poke mpp nullPtr
    poke dpp nullPtr
    poke sidp nullPtr
    poke eidp nullPtr
    poke uuidpp nullPtr
    liftIO $ c_fromTaskStatus s tidp sp mpp mlp sourcesp sourcep reasonsp reasonp dpp dlp sidp eidp tssp tsp uuidpp uuidlp hsp hp
    tid <- unmarshal =<< peek tidp
    state <- (toEnum . fromIntegral) <$> peek sp
    msg <- peekMaybeBS mpp mlp
    source <- fmap (toEnum . fromIntegral . (\(CInt x)-> x))
              <$> peekMaybePrim sourcep sourcesp
    reason <- fmap (toEnum . fromIntegral . (\(CInt x)-> x))
              <$> peekMaybePrim reasonp reasonsp
    dat <- peekMaybeBS dpp dlp
    sid <- peekMaybeCPP sidp
    eid <- peekMaybeCPP eidp
    h <- peekMaybePrim hp hsp
    uuid <- peekMaybeBS uuidpp uuidlp
    ts <- fmap (\(CDouble d) -> d) <$> peekMaybePrim tsp tssp
    return $ TaskStatus tid state msg source reason dat sid eid ts uuid (fmap fromCBool h)

  destroy = c_destroyTaskStatus
