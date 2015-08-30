module System.Mesos.Raw.ResourceUsageExecutor where
import           System.Mesos.Internal
import           System.Mesos.Raw.ExecutorInfo
import           System.Mesos.Raw.Resource
import           System.Mesos.Raw.ResourceStatistics

type ResourceUsageExecutorPtr = Ptr ResourceUsageExecutor

foreign import ccall unsafe "ext/types.h toResourceUsage_Executor" c_toResourceUsage_Executor
  :: ExecutorInfoPtr
  -> Ptr ResourcePtr
  -> CInt
  -> ResourceStatisticsPtr
  -> IO ResourceUsageExecutorPtr

foreign import ccall unsafe "ext/types.h fromResourceUsage_Executor" c_fromResourceUsage_Executor
  :: ResourceUsageExecutorPtr
  -> Ptr ExecutorInfoPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr ResourceStatisticsPtr
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyResourceUsage" c_destroyResourceUsage_Executor
  :: ResourceUsageExecutorPtr
  -> IO ()

instance CPPValue ResourceUsageExecutor where
  marshal ex = do
    exeInfoP <- cppValue $ resourceUsageExecutorExecutorInfo ex
    res <- mapM cppValue $ resourceUsageExecutorAllocated ex
    (resP, resl) <- arrayLen res
    statsP <- maybe (return nullPtr) cppValue $ resourceUsageExecutorStatistics ex
    liftIO $ c_toResourceUsage_Executor exeInfoP
               resP (fromIntegral resl) statsP
  unmarshal ex = do
    exeInfoPP <- alloc
    statsPP <- alloc
    (resPP, resl) <- arrayPair
    poke statsPP nullPtr
    poke resPP nullPtr
    liftIO $ c_fromResourceUsage_Executor ex exeInfoPP resPP resl statsPP
    exeInfoP <- peek exeInfoPP
    exeInfo <- peekCPP exeInfoP
    resP <- peek resPP
    resources <- mapM unmarshal =<< peekArray (resP, resl)
    stats <- peekMaybeCPP statsPP
    return $ ResourceUsageExecutor exeInfo resources stats
  destroy = c_destroyResourceUsage_Executor
