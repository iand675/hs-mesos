module System.Mesos.Raw.FrameworkId where
import           System.Mesos.Internal

type FrameworkIDPtr = Ptr FrameworkID

foreign import ccall unsafe "ext/types.h toFrameworkID" c_toFrameworkID :: ToID FrameworkIDPtr

foreign import ccall unsafe "ext/types.h fromFrameworkID" c_fromFrameworkID :: FromID FrameworkIDPtr

foreign import ccall unsafe "ext/types.h destroyFrameworkID" c_destroyFrameworkID :: FrameworkIDPtr -> IO ()

instance CPPValue FrameworkID where
  marshal x = do
    (strp, l) <- cstring $ fromFrameworkID x
    liftIO $ c_toFrameworkID strp (fromIntegral l)

  unmarshal p = fmap FrameworkID $ do
    ptrPtr <- alloc
    len <- liftIO $ c_fromFrameworkID p ptrPtr
    peekCString' (ptrPtr, len)

  destroy = c_destroyFrameworkID
