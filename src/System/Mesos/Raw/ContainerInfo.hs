{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.ContainerInfo where

import           System.Mesos.Internal
import           System.Mesos.Raw.Volume

type ContainerInfoPtr = Ptr ContainerInfo

foreign import ccall unsafe "ext/types.h toContainerInfo" c_toContainerInfo
  :: CInt -- ^ type
  -> Ptr CChar -- ^ image
  -> CInt -- ^ image length
  -> Ptr VolumePtr -- ^ volumes
  -> CInt -- ^ volumes length
  -> IO ContainerInfoPtr

foreign import ccall unsafe "ext/types.h fromContainerInfo" c_fromContainerInfo
  :: ContainerInfoPtr
  -> Ptr CInt -- ^ type
  -> Ptr (Ptr CChar) -- ^ image
  -> Ptr CInt -- ^ image length
  -> Ptr (Ptr VolumePtr)
  -> Ptr CInt -- ^ volumes length
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyContainerInfo" c_destroyContainerInfo
  :: ContainerInfoPtr -> IO ()

instance CPPValue ContainerInfo where
  marshal ci = do
    (ip, il) <- cstring $ dockerImage $ containerInfoContainerType ci
    (vp, vl) <- mapM marshal (containerInfoVolumes ci) >>= arrayLen
    liftIO $ c_toContainerInfo 1 ip (fromIntegral il) vp (fromIntegral vl)

  unmarshal p = do
    tyP <- alloc
    (iP, ilP) <- arrayPair
    vPP <- alloc
    vlP <- alloc
    liftIO $ c_fromContainerInfo p tyP iP ilP vPP vlP
    ty <- peek tyP
    im <- peekMaybeCString (iP, ilP)
    vP <- peek vPP
    vps <- peekArray (vP, vlP)
    vs <- mapM unmarshal vps
    if ty == 1
       then return $ ContainerInfo (Docker $ maybe "" id im) vs
       else return $ ContainerInfo (Unknown $ fromIntegral ty)  vs

  destroy = c_destroyContainerInfo
