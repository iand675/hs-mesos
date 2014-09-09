module System.Mesos.Raw.Request where
import           System.Mesos.Internal
import           System.Mesos.Raw.Resource
import           System.Mesos.Raw.SlaveId

type RequestPtr = Ptr Request

foreign import ccall unsafe "ext/types.h toRequest" c_toRequest
  :: SlaveIDPtr
  -> Ptr ResourcePtr
  -> CInt
  -> IO RequestPtr

foreign import ccall unsafe "ext/types.h fromRequest" c_fromRequest
  :: RequestPtr
  -> Ptr SlaveIDPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyRequest" c_destroyRequest
  :: RequestPtr
  -> IO ()

instance CPPValue Request where
  marshal r = do
    sp <- maybe (return nullPtr) cppValue $ requestSlaveID r
    rps <- mapM cppValue $ reqResources r
    (rpp, rl) <- arrayLen rps
    liftIO $ c_toRequest sp rpp (fromIntegral rl)

  unmarshal r = do
    spp <- alloc
    rpp <- alloc
    rlp <- alloc
    poke spp nullPtr
    liftIO $ c_fromRequest r spp rpp rlp
    sp <- peek spp
    rp <- peek rpp
    rps <- peekArray (rp, rlp)
    rs <- mapM unmarshal rps
    s <- if sp == nullPtr
      then return Nothing
      else fmap Just $ unmarshal sp
    return $ Request s rs

  destroy = c_destroyRequest

  equalExceptDefaults (Request sid rs) (Request sid' rs') = sid == sid' && and (zipWith equalExceptDefaults rs rs')
