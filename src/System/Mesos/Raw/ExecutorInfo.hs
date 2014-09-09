module System.Mesos.Raw.ExecutorInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.CommandInfo
import           System.Mesos.Raw.ContainerInfo
import           System.Mesos.Raw.ExecutorId
import           System.Mesos.Raw.FrameworkId
import           System.Mesos.Raw.Resource

type ExecutorInfoPtr = Ptr ExecutorInfo

foreign import ccall unsafe "ext/types.h toExecutorInfo" c_toExecutorInfo
  :: ExecutorIDPtr
  -> FrameworkIDPtr
  -> CommandInfoPtr
  -> ContainerInfoPtr
  -> Ptr ResourcePtr  -- ^ resources
  -> CInt      -- ^ resource count
  -> Ptr CChar -- ^ name
  -> CInt      -- ^ name length
  -> Ptr CChar -- ^ source
  -> CInt      -- ^ source length
  -> Ptr CChar -- ^ data
  -> CInt      -- ^ data length
  -> IO ExecutorInfoPtr

foreign import ccall unsafe "ext/types.h fromExecutorInfo" c_fromExecutorInfo
  :: ExecutorInfoPtr
  -> Ptr ExecutorIDPtr
  -> Ptr FrameworkIDPtr
  -> Ptr CommandInfoPtr
  -> Ptr ContainerInfoPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyExecutorInfo" c_destroyExecutorInfo
  :: ExecutorInfoPtr
  -> IO ()

instance CPPValue ExecutorInfo where

  marshal i = do
    eidP <- cppValue $ executorInfoExecutorID i
    fidP <- cppValue $ executorInfoFrameworkID i
    ciP <- cppValue $ executorInfoCommandInfo i
    ctrP <- case executorInfoContainerInfo i of
              Nothing -> return nullPtr
              Just ctr -> cppValue ctr
    rps <- mapM cppValue $ executorInfoResources i
    (np, nl) <- maybeCString $ executorName i
    (sp, sl) <- maybeCString $ executorSource i
    (dp, dl) <- maybeCString $ executorData i
    (rs, rLen) <- arrayLen rps
    liftIO $ c_toExecutorInfo eidP fidP ciP ctrP rs (fromIntegral rLen) np (fromIntegral nl) sp (fromIntegral sl) dp (fromIntegral dl)

  unmarshal ip = do
    eidP <- alloc
    fidP <- alloc
    ciP <- alloc
    cntP <- alloc
    rpP <- alloc
    rpL <- alloc
    enP <- alloc
    enL <- alloc
    esP <- alloc
    esL <- alloc
    edP <- alloc
    edL <- alloc
    poke enP nullPtr
    poke esP nullPtr
    poke edP nullPtr
    poke cntP nullPtr
    liftIO $ c_fromExecutorInfo ip eidP fidP ciP cntP rpP rpL enP enL esP esL edP edL
    eid <- unmarshal =<< peek eidP
    fid <- unmarshal =<< peek fidP
    ci <- unmarshal =<< peek ciP
    cnt <- peekMaybeCPP cntP
    rl <- peek rpL
    rp <- peek rpP
    rps <- peekArray' (rp, fromIntegral rl)
    rs <- mapM unmarshal rps
    en <- peekMaybeBS enP enL
    es <- peekMaybeBS esP esL
    ed <- peekMaybeBS edP edL
    return $ ExecutorInfo eid fid ci cnt rs en es ed

  destroy = c_destroyExecutorInfo

  equalExceptDefaults (ExecutorInfo id fid ci cnt rs n s d) (ExecutorInfo id' fid' ci' cnt' rs' n' s' d') =
    id == id' && fid == fid' && equalExceptDefaults ci ci' && and (zipWith equalExceptDefaults rs rs') && n == n' && s == s' && d == d' && cnt == cnt'
