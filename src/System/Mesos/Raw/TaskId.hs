module System.Mesos.Raw.TaskId where
import           System.Mesos.Internal

type TaskIDPtr = Ptr TaskID

foreign import ccall "ext/types.h toTaskID" c_toTaskID :: ToID TaskIDPtr

foreign import ccall "ext/types.h fromTaskID" c_fromTaskID :: FromID TaskIDPtr

foreign import ccall "ext/types.h destroyTaskID" c_destroyTaskID :: TaskIDPtr -> IO ()

instance CPPValue TaskID where
  marshal x = do
    (strp, l) <- cstring $ fromTaskID x
    liftIO $ c_toTaskID strp $ fromIntegral l

  unmarshal p = fmap TaskID $ do
    pp <- alloc
    len <- liftIO $ c_fromTaskID p pp
    peekCString' (pp, len)

  destroy = c_destroyTaskID
