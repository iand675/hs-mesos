module System.Mesos.Raw.ResourceUsage where
import           System.Mesos.Internal
import           System.Mesos.Raw.ResourceUsageExecutor

type ResourceUsagePtr = Ptr ResourceUsage

foreign import ccall unsafe "ext/types.h toResourceUsage" c_toResourceUsage
  :: Ptr ResourceUsageExecutorPtr
  -> CInt
  -> IO ResourceUsagePtr

foreign import ccall unsafe "ext/types.h fromResourceUsage" c_fromResourceUsage
  :: ResourceUsagePtr
  -> Ptr (Ptr ResourceUsageExecutorPtr)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyResourceUsage" c_destroyResourceUsage
  :: ResourceUsagePtr
  -> IO ()

instance CPPValue ResourceUsage where
  marshal res = do
    exes <- mapM cppValue (resourceUsageExecutors res)
    (exesP, exesl) <- arrayLen exes
    liftIO $ c_toResourceUsage exesP (fromIntegral exesl)

  unmarshal up = do
    (exesPP, exesl) <- arrayPair
    liftIO $ c_fromResourceUsage up exesPP exesl
    exesP <- peek exesPP
    exes <- mapM unmarshal =<< peekArray (exesP, exesl)
    return $ ResourceUsage exes

  destroy = c_destroyResourceUsage

  equalExceptDefaults (ResourceUsage exes) (ResourceUsage exes') =
    and $ zipWith equalExceptDefaults exes exes'
