{-# LANGUAGE OverloadedStrings #-}
module System.Mesos.Raw.DiscoveryInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.Port
import           System.Mesos.Raw.Label

type DiscoveryInfoPtr = Ptr DiscoveryInfo

foreign import ccall unsafe "ext/types.h toDiscoveryInfo" c_toDiscoveryInfo
  :: CInt -- ^ visibility
  -> Ptr CChar -- ^ name
  -> CInt -- ^ name length
  -> Ptr CChar -- ^ environment
  -> CInt -- ^ environment length
  -> Ptr CChar -- ^ location
  -> CInt -- ^ location length
  -> Ptr CChar -- ^ version
  -> CInt -- ^ version length
  -> Ptr PortPtr -- ^ ports
  -> CInt -- ^ port count
  -> Ptr LabelPtr -- ^ labels
  -> CInt -- ^ label count
  -> IO DiscoveryInfoPtr

foreign import ccall unsafe "ext/types.h fromDiscoveryInfo" c_fromDiscoveryInfo
  :: DiscoveryInfoPtr
  -> Ptr CInt -- ^ visibility
  -> Ptr (Ptr CChar) -- ^ name
  -> Ptr CInt -- ^ name length
  -> Ptr (Ptr CChar) -- ^ environment
  -> Ptr CInt -- ^ environment length
  -> Ptr (Ptr CChar) -- ^ location
  -> Ptr CInt -- ^ location length
  -> Ptr (Ptr CChar) -- ^ version
  -> Ptr CInt -- ^ version length
  -> Ptr (Ptr PortPtr) -- ^ ports
  -> Ptr CInt -- ^ port count
  -> Ptr (Ptr LabelPtr) -- ^ labels
  -> Ptr CInt -- ^ label count
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyDiscoveryInfo" c_destroyDiscoveryInfo
  :: DiscoveryInfoPtr
  -> IO ()

instance CPPValue DiscoveryInfo where
  marshal d = do
    (nameP, namel) <- maybeCString $ discoveryInfoName d
    (envP, envl) <- maybeCString $ discoveryInfoEnvironment d
    (locP, locl) <- maybeCString $ discoveryInfoLocation d
    (verP, verl) <- maybeCString $ discoveryInfoVersion d
    (portsP, portsl) <- mapM marshal (discoveryInfoPorts d)
                        >>= arrayLen
    (labelsP, labelsl) <- mapM (marshal . toLabel)
                          (discoveryInfoLabels d)
                       >>= arrayLen

    liftIO $ c_toDiscoveryInfo
      (fromIntegral . fromEnum $ discoveryInfoVisibility d)
      nameP (fromIntegral namel)
      envP (fromIntegral envl)
      locP (fromIntegral locl)
      verP (fromIntegral verl)
      portsP (fromIntegral portsl)
      labelsP (fromIntegral labelsl)

  unmarshal d = do
    visP <- alloc
    (namePP, namelP) <- arrayPair
    (envPP, envlP) <- arrayPair
    (locPP, loclP) <- arrayPair
    (verPP, verlP) <- arrayPair
    poke namePP nullPtr
    poke envPP nullPtr
    poke locPP nullPtr
    poke verPP nullPtr
    portsPP <- alloc
    portslP <- alloc
    labelsPP <- alloc
    labelslP <- alloc
    liftIO $ c_fromDiscoveryInfo d visP
               namePP namelP
               envPP envlP
               locPP loclP
               verPP verlP
               portsPP portslP
               labelsPP labelslP

    vis <- peek visP
    name <- peekMaybeBS namePP namelP
    env <- peekMaybeBS envPP envlP
    loc <- peekMaybeBS locPP loclP
    ver <- peekMaybeBS verPP verlP
    portsP <- peek portsPP
    ports' <- peekArray (portsP, portslP)
    ports <- mapM unmarshal ports'
    labelsP <- peek labelsPP
    labels' <- peekArray (labelsP, labelslP)
    labels <- mapM unmarshal labels'
    return $ DiscoveryInfo (toEnum (fromIntegral vis)) name env
               loc ver ports (map fromLabel labels)

  destroy = c_destroyDiscoveryInfo
