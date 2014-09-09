module System.Mesos.Raw.SlaveInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.Attribute
import           System.Mesos.Raw.Resource
import           System.Mesos.Raw.SlaveId

type SlaveInfoPtr = Ptr SlaveInfo

foreign import ccall unsafe "ext/types.h toSlaveInfo" c_toSlaveInfo
  :: Ptr CChar
  -> CInt
  -> Ptr CUInt
  -> Ptr ResourcePtr
  -> CInt
  -> Ptr AttributePtr
  -> CInt
  -> SlaveIDPtr
  -> Ptr CBool
  -> IO SlaveInfoPtr

foreign import ccall unsafe "ext/types.h fromSlaveInfo" c_fromSlaveInfo
  :: SlaveInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CBool
  -> Ptr CUInt
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr (Ptr AttributePtr)
  -> Ptr CInt
  -> Ptr SlaveIDPtr
  -> Ptr CBool
  -> Ptr CBool
  -> IO ()

foreign import ccall unsafe "ext/types.h destroySlaveInfo" c_destroySlaveInfo
  :: SlaveInfoPtr
  -> IO ()

instance CPPValue SlaveInfo where
  marshal i = do
    (hp, hl) <- cstring $ slaveInfoHostname i
    pp <- allocMaybe (CUInt <$> slaveInfoPort i)
    cp <- allocMaybe (toCBool <$> slaveInfoCheckpoint i)
    (rp, rl) <- arrayLen =<< mapM cppValue (slaveInfoResources i)
    (ap, al) <- arrayLen =<< mapM (cppValue . toAttribute) (slaveInfoAttributes i)
    sidp <- maybe (return nullPtr) cppValue $ slaveInfoSlaveID i
    liftIO $ c_toSlaveInfo hp (fromIntegral hl) pp rp (fromIntegral rl) ap (fromIntegral al) sidp cp

  unmarshal i = do
    hstr@(hpp, hlp) <- arrayPair
    pps <- alloc
    pp <- alloc
    (rpp, rlp) <- arrayPair
    (app, alp) <- arrayPair
    ipp <- alloc
    cps <- alloc
    cp <- alloc
    poke ipp nullPtr
    liftIO $ c_fromSlaveInfo i hpp hlp pps pp rpp rlp app alp ipp cps cp
    h <- peekCString hstr
    p <- peekMaybePrim pp pps
    rp <- peek rpp
    rs <- mapM unmarshal =<< peekArray (rp, rlp)
    ap <- peek app
    as <- mapM unmarshal =<< peekArray (ap, alp)
    ip <- peek ipp
    sid <- if ip == nullPtr
      then return Nothing
      else fmap Just $ unmarshal ip
    c <- peekMaybePrim cp cps
    return $ SlaveInfo h (fromIntegral <$> p) rs (map fromAttribute as) sid $ fmap fromCBool c

  destroy = c_destroySlaveInfo

  equalExceptDefaults (SlaveInfo hn p rs as sid cp) (SlaveInfo hn' p' rs' as' sid' cp') =
    (hn == hn') &&
    (p == p' || p == Just 5051) &&
    (and $ zipWith equalExceptDefaults rs rs') &&
    (and $ zipWith (\(k, v) (k', v') -> k == k' && equalExceptDefaults v v') as as') &&
    (sid == sid') &&
    defEq False cp cp'
