module System.Mesos.Raw.ExecutorId where
import           System.Mesos.Internal

type ExecutorIDPtr = Ptr ExecutorID

foreign import ccall unsafe "ext/types.h toExecutorID" c_toExecutorID :: ToID ExecutorIDPtr

foreign import ccall unsafe "ext/types.h fromExecutorID" c_fromExecutorID :: FromID ExecutorIDPtr

foreign import ccall unsafe "ext/types.h destroyExecutorID" c_destroyExecutorID :: ExecutorIDPtr -> IO ()

instance CPPValue ExecutorID where
  marshal x = do
    (strp, l) <- cstring $ fromExecutorID x
    liftIO $ c_toExecutorID strp $ fromIntegral l

  unmarshal p = fmap ExecutorID $ do
    ptrPtr <- alloc
    len <- liftIO $ c_fromExecutorID p ptrPtr
    peekCString' (ptrPtr, len)

  destroy = c_destroyExecutorID
