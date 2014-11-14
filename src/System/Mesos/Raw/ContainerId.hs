module System.Mesos.Raw.ContainerId where
import           System.Mesos.Internal

type ContainerIDPtr = Ptr ContainerID

foreign import ccall unsafe "ext/types.h toContainerID" c_toContainerID :: ToID ContainerIDPtr

foreign import ccall unsafe "ext/types.h fromContainerID" c_fromContainerID :: FromID ContainerIDPtr

foreign import ccall unsafe "ext/types.h destroyContainerID" c_destroyContainerID :: ContainerIDPtr -> IO ()

instance CPPValue ContainerID where

  marshal x = do
    (strp, l) <- cstring $ containerIDId' x
    liftIO $ c_toContainerID strp $ fromIntegral l

  unmarshal p = fmap ContainerID $ do
    ptrPtr <- alloc
    len <- liftIO $ c_fromContainerID p ptrPtr
    peekCString' (ptrPtr, len)

  destroy = c_destroyContainerID
