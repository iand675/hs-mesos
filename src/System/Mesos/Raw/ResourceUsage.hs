module System.Mesos.Raw.ResourceUsage where
import           System.Mesos.Internal
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.FrameworkId
import           System.Mesos.Raw.ResourceStatistics
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.TaskId

type ResourceUsagePtr = Ptr ResourceUsage

foreign import ccall unsafe "ext/types.h toResourceUsage" c_toResourceUsage
  :: SlaveIDPtr
  -> FrameworkIDPtr
  -> ExecutorIDPtr
  -> Ptr CChar
  -> CInt
  -> TaskIDPtr
  -> ResourceStatisticsPtr
  -> IO ResourceUsagePtr

foreign import ccall unsafe "ext/types.h fromResourceUsage" c_fromResourceUsage
  :: ResourceUsagePtr
  -> Ptr SlaveIDPtr
  -> Ptr FrameworkIDPtr
  -> Ptr ExecutorIDPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr TaskIDPtr
  -> Ptr ResourceStatisticsPtr
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyResourceUsage" c_destroyResourceUsage
  :: ResourceUsagePtr
  -> IO ()

instance CPPValue ResourceUsage where
  marshal (ResourceUsage sid fid eid en tid rs) = do
    sidP <- cppValue sid
    fidP <- cppValue fid
    eidP <- maybe (return nullPtr) cppValue eid
    tidP <- maybe (return nullPtr) cppValue tid
    rsP <- maybe (return nullPtr) cppValue rs
    (enP, enL) <- maybeCString en
    liftIO $ c_toResourceUsage sidP fidP eidP enP (fromIntegral enL) tidP rsP

  unmarshal up = do
    sidPP <- alloc
    fidPP <- alloc
    eidPP <- alloc
    enPP <- alloc
    enLP <- alloc
    tidPP <- alloc
    rsPP <- alloc
    poke eidPP nullPtr
    poke enPP nullPtr
    poke tidPP nullPtr
    poke rsPP nullPtr
    liftIO $ c_fromResourceUsage up sidPP fidPP eidPP enPP enLP tidPP rsPP
    sid <- unmarshal =<< peek sidPP
    fid <- unmarshal =<< peek fidPP
    eidP <- peek eidPP
    eid <- if eidP == nullPtr
             then return Nothing
             else Just <$> unmarshal eidP
    en <- peekMaybeBS enPP enLP
    tidP <- peek tidPP
    tid <- if tidP == nullPtr
             then return Nothing
             else Just <$> unmarshal tidP
    rsP <- peek rsPP
    rs <- if rsP == nullPtr
            then return Nothing
            else Just <$> unmarshal rsP
    return $ ResourceUsage sid fid eid en tid rs

  destroy = c_destroyResourceUsage
