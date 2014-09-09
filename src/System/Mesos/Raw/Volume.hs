module System.Mesos.Raw.Volume where

import           System.Mesos.Internal

type VolumePtr = Ptr Volume

foreign import ccall unsafe "ext/types.h toVolume" c_toVolume
  :: Ptr CChar -- ^ container_path
  -> CInt      -- ^ container_path length
  -> Ptr CChar -- ^ host_path
  -> CInt      -- ^ host_path length
  -> CInt      -- ^ mode
  -> IO VolumePtr

foreign import ccall unsafe "ext/types.h fromVolume" c_fromVolume
  :: VolumePtr
  -> Ptr (Ptr CChar) -- ^ container_path
  -> Ptr CInt        -- ^ container_path length
  -> Ptr (Ptr CChar) -- ^ host_path
  -> Ptr CInt        -- ^ host_path length
  -> Ptr CInt        -- ^ mode
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyVolume" c_destroyVolume
  :: VolumePtr -> IO ()

instance CPPValue Volume where
  marshal v = do
    (cp, cl) <- cstring $ volumeContainerPath v
    (vp, vl) <- maybeCString $ volumeHostPath v
    liftIO $ c_toVolume cp (fromIntegral cl) vp (fromIntegral vl) $ fromIntegral $ fromEnum $ volumeMode v

  unmarshal p = do
    (cpp, clp) <- arrayPair
    (vpp, vlp) <- arrayPair
    mp <- alloc
    liftIO $ c_fromVolume p cpp clp vpp vlp mp
    c <- peekCString (cpp, clp)
    v <- peekMaybeCString (vpp, vlp)
    m <- fmap (toEnum . fromIntegral) $ peek mp
    return $ Volume c v m

  destroy = c_destroyVolume
