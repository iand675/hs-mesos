module System.Mesos.Raw.SlaveId where
import           System.Mesos.Internal

type SlaveIDPtr = Ptr SlaveID

foreign import ccall unsafe "ext/types.h toSlaveID" c_toSlaveID :: ToID SlaveIDPtr

foreign import ccall unsafe "ext/types.h fromSlaveID" c_fromSlaveID :: FromID SlaveIDPtr

foreign import ccall unsafe "ext/types.h destroySlaveID" c_destroySlaveID :: SlaveIDPtr -> IO ()

instance CPPValue SlaveID where
  marshal x = do
    (strp, l) <- cstring $ fromSlaveID x
    liftIO $ c_toSlaveID strp $ fromIntegral l

  unmarshal p = fmap SlaveID $ do
    ptrPtr <- alloc
    len <- liftIO $ c_fromSlaveID p ptrPtr
    peekCString' (ptrPtr, len)

  destroy = c_destroySlaveID
