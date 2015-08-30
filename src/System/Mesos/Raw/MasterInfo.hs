module System.Mesos.Raw.MasterInfo where
import           System.Mesos.Internal

type MasterInfoPtr = Ptr MasterInfo

foreign import ccall unsafe "ext/types.h toMasterInfo" c_toMasterInfo
  :: Ptr CChar -- infoID
  -> CInt -- infoIDLen
  -> CUInt -- infoIP
  -> Ptr CUInt -- infoPort
  -> Ptr CChar -- pid
  -> CInt -- pidLen
  -> Ptr CChar -- hostname
  -> CInt -- hostnameLen
  -> Ptr CChar -- version
  -> CInt -- versionLen
  -> IO MasterInfoPtr

foreign import ccall unsafe "ext/types.h fromMasterInfo" c_fromMasterInfo
  :: MasterInfoPtr -- info
  -> Ptr (Ptr CChar) -- infoId
  -> Ptr CInt -- infoIdlen
  -> Ptr CUInt -- infoIP
  -> Ptr CUInt -- infoPort
  -> Ptr (Ptr CChar) -- pid
  -> Ptr CInt -- pidLen
  -> Ptr (Ptr CChar) -- hostname
  -> Ptr CInt -- hostnameLen
  -> Ptr (Ptr CChar) -- version
  -> Ptr CInt -- versionLen
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyMasterInfo" c_destroyMasterInfo
  :: MasterInfoPtr
  -> IO ()

instance CPPValue MasterInfo where
  marshal i = do
    (idp, idl) <- cstring $ masterInfoId' i
    (pidp, pidl) <- maybeCString $ masterInfoPid i
    (hnp, hnl) <- maybeCString $ masterInfoHostname i
    (verp, verl) <- maybeCString $ masterInfoVersion i
    prt <- allocMaybe $ fmap CUInt $ masterInfoPort i
    liftIO $ c_toMasterInfo idp (fromIntegral idl) (CUInt $ masterInfoIp i) prt pidp (fromIntegral pidl) hnp (fromIntegral hnl) verp (fromIntegral verl)

  unmarshal i = do
    (idpP, idlP) <- arrayPair
    ipP <- alloc
    portP <- alloc
    (pidpP, pidlP) <- arrayPair
    (hnpP, hnlP) <- arrayPair
    (verpP, verlP) <- arrayPair
    poke pidpP nullPtr
    poke hnpP nullPtr
    poke verpP nullPtr
    liftIO $ c_fromMasterInfo i idpP idlP ipP portP pidpP pidlP hnpP hnlP verpP verlP
    mID <- peekCString (idpP, idlP)
    (CUInt ip) <- peek ipP
    (CUInt port) <- peek portP
    pid <- peekMaybeBS pidpP pidlP
    hn <- peekMaybeBS hnpP hnlP
    version <- peekMaybeBS verpP verlP
    return $ MasterInfo mID ip (Just port) pid hn version

  destroy = c_destroyMasterInfo

  equalExceptDefaults (MasterInfo mID ip p pid hn ver) (MasterInfo mID' ip' p' pid' hn' ver') = mID == mID' && ip == ip' && defEq 5050 p p' && pid == pid' && hn == hn' && ver == ver'
