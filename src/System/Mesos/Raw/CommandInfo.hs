module System.Mesos.Raw.CommandInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.CommandUri
import           System.Mesos.Raw.Environment
import           System.Mesos.Raw.StdString

type CommandInfoPtr = Ptr CommandInfo

foreign import ccall unsafe "ext/types.h toCommandInfo" c_toCommandInfo
  :: Ptr CommandURIPtr
  -> CInt
  -> EnvironmentPtr
  -> CBool
  -> Ptr CChar
  -> CInt
  -> Ptr StdStringPtr
  -> CInt
  -> Ptr CChar
  -> CInt
  -> IO CommandInfoPtr

foreign import ccall unsafe "ext/types.h fromCommandInfo" c_fromCommandInfo
  :: CommandInfoPtr
  -> Ptr (Ptr CommandURIPtr)
  -> Ptr CInt
  -> Ptr EnvironmentPtr
  -> Ptr CBool
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr StdStringPtr)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyCommandInfo" c_destroyCommandInfo
  :: CommandInfoPtr
  -> IO ()

instance CPPValue CommandInfo where

  marshal i = do
    envP <- maybe (return nullPtr) (cppValue . toEnvironment) $ commandEnvironment i
    uriPs <- mapM cppValue $ commandInfoURIs i
    (upp, upl) <- arrayLen uriPs
    (up, ul) <- maybeCString $ commandUser i
    case commandValue i of
      (ShellCommand cmd) -> do
        (vp, vl) <- cstring cmd
        liftIO $ c_toCommandInfo upp (fromIntegral upl) envP (toCBool True) vp (fromIntegral vl) nullPtr 0 up (fromIntegral ul)
      (RawCommand cmd args) -> do
        (vp, vl) <- cstring cmd
        (ap, al) <- arrayLen =<< mapM (cppValue . StdString) args
        liftIO $ c_toCommandInfo upp (fromIntegral upl) envP (toCBool False) vp (fromIntegral vl) ap (fromIntegral al) up (fromIntegral ul)

  unmarshal i = do
    upp <- alloc
    ulp <- alloc
    vl@(vpp, vlp) <- arrayPair
    epp <- alloc
    poke epp nullPtr
    aspp <- alloc
    asl <- alloc
    u@(up, ul) <- arrayPair
    poke up nullPtr
    sp <- alloc
    liftIO $ c_fromCommandInfo i upp ulp epp sp vpp vlp aspp asl up ul
    ups <- peek upp
    us <- mapM unmarshal =<< peekArray (ups, ulp)
    e <- peekMaybeCPP epp
    s <- fmap fromCBool $ peek sp
    val <- peekCString vl
    v <- if s
           then return $ ShellCommand val
           else do
             asp <- peek aspp
             args <- mapM unmarshal =<< peekArray (asp, asl)
             return $ RawCommand val $ fmap fromStdString args
    user <- peekMaybeCString u
    return $ CommandInfo us (fmap fromEnvironment e) v user

  destroy = c_destroyCommandInfo
