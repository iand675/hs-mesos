module System.Mesos.Raw.MasterInfo where
import           System.Mesos.Internal

type MasterInfoPtr = Ptr MasterInfo

foreign import ccall unsafe "ext/types.h toMasterInfo" c_toMasterInfo
  :: Ptr CChar
  -> CInt
  -> CUInt
  -> Ptr CUInt
  -> Ptr CChar
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO MasterInfoPtr

foreign import ccall unsafe "ext/types.h fromMasterInfo" c_fromMasterInfo
  :: MasterInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr CUInt
  -> Ptr CUInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyMasterInfo" c_destroyMasterInfo
  :: MasterInfoPtr
  -> IO ()

instance CPPValue MasterInfo where
  marshal i = do
    (idp, idl) <- cstring $ masterInfoID i
    (pidp, pidl) <- maybeCString $ masterInfoPID i
    (hnp, hnl) <- maybeCString $ masterInfoHostname i
    prt <- allocMaybe $ fmap CUInt $ masterInfoPort i
    liftIO $ c_toMasterInfo idp (fromIntegral idl) (CUInt $ masterInfoIP i) prt pidp (fromIntegral pidl) hnp (fromIntegral hnl)

  unmarshal i = do
    (idpP, idlP) <- arrayPair
    ipP <- alloc
    portP <- alloc
    (pidpP, pidlP) <- arrayPair
    (hnpP, hnlP) <- arrayPair
    poke pidpP nullPtr
    poke hnpP nullPtr
    liftIO $ c_fromMasterInfo i idpP idlP ipP portP pidpP pidlP hnpP hnlP
    mID <- peekCString (idpP, idlP)
    (CUInt ip) <- peek ipP
    (CUInt port) <- peek portP
    pid <- peekMaybeBS pidpP pidlP
    hn <- peekMaybeBS hnpP hnlP
    return $ MasterInfo mID ip (Just port) pid hn

  destroy = c_destroyMasterInfo

  equalExceptDefaults (MasterInfo mID ip p pid hn) (MasterInfo mID' ip' p' pid' hn') = mID == mID' && ip == ip' && defEq 5050 p p' && pid == pid' && hn == hn'
