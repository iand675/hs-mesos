module System.Mesos.Raw.TaskInfo where
import           System.Mesos.Internal
import           System.Mesos.Raw.CommandInfo
import           System.Mesos.Raw.ContainerInfo
import           System.Mesos.Raw.ExecutorInfo
import           System.Mesos.Raw.HealthCheck
import           System.Mesos.Raw.Resource
import           System.Mesos.Raw.SlaveId
import           System.Mesos.Raw.TaskId

type TaskInfoPtr = Ptr TaskInfo

foreign import ccall unsafe "ext/types.h toTaskInfo" c_toTaskInfo
  :: Ptr CChar
  -> CInt
  -> TaskIDPtr
  -> SlaveIDPtr
  -> Ptr ResourcePtr
  -> CInt
  -> ExecutorInfoPtr
  -> CommandInfoPtr
  -> Ptr CChar
  -> CInt
  -> ContainerInfoPtr
  -> HealthCheckPtr
  -> IO TaskInfoPtr

foreign import ccall unsafe "ext/types.h fromTaskInfo" c_fromTaskInfo
  :: TaskInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr TaskIDPtr
  -> Ptr SlaveIDPtr
  -> Ptr (Ptr ResourcePtr)
  -> Ptr CInt
  -> Ptr ExecutorInfoPtr
  -> Ptr CommandInfoPtr
  -> Ptr (Ptr CChar)
  -> Ptr CInt
  -> Ptr ContainerInfoPtr
  -> Ptr HealthCheckPtr
  -> IO ()

foreign import ccall unsafe "ext/types.h destroyTaskInfo" c_destroyTaskInfo
  :: TaskInfoPtr
  -> IO ()

instance CPPValue TaskInfo where

  marshal t = do
    (np, nl) <- cstring $ taskName t
    rps <- mapM cppValue (taskResources t)
    tid <- cppValue $ taskID t
    sid <- cppValue $ taskSlaveID t
    eip <- maybe (return nullPtr) cppValue $ case taskImplementation t of
                                               TaskExecutor e -> Just e
                                               _ -> Nothing
    cip <- maybe (return nullPtr) cppValue $ case taskImplementation t of
                                               TaskCommand c -> Just c
                                               _ -> Nothing
    (tdp, tdl) <- maybeCString $ taskData t
    (rpp, rl) <- arrayLen rps
    ctrp <- maybe (return nullPtr) cppValue $ taskContainer t
    hcp <- maybe (return nullPtr) cppValue $ taskHealthCheck t
    liftIO $ c_toTaskInfo np (fromIntegral nl) tid sid rpp (fromIntegral rl) eip cip tdp (fromIntegral tdl) ctrp hcp

  unmarshal t = do
    npp <- alloc
    nlp <- alloc
    tpp <- alloc
    spp <- alloc
    rpp <- alloc
    rlp <- alloc
    epp <- alloc
    cpp <- alloc
    dpp <- alloc
    dlp <- alloc
    cip <- alloc
    hcp <- alloc
    poke epp nullPtr
    poke cpp nullPtr
    poke dpp nullPtr
    poke cip nullPtr
    poke hcp nullPtr
    liftIO $ c_fromTaskInfo t npp nlp tpp spp rpp rlp epp cpp dpp dlp cip hcp
    n <- peekCString (npp, nlp)
    tp <- peek tpp
    t <- peekCPP tp
    sp <- peek spp
    s <- peekCPP sp
    rp <- peek rpp
    rs <- mapM peekCPP =<< peekArray (rp, rlp)
    ep <- peek epp
    e <- if ep == nullPtr
      then return Nothing
      else fmap Just $ unmarshal ep
    cp <- peek cpp
    c <- if cp == nullPtr
      then return Nothing
      else fmap Just $ unmarshal cp
    -- this shouldn't ever happen, so it's probably ok to just have an error here.
    let ei = maybe (maybe (error "FATAL: TaskInfo must have CommandInfo or ExecutorInfo") TaskCommand c) TaskExecutor e
    d <- peekMaybeBS dpp dlp
    ci <- peekMaybeCPP cip
    hc <- peekMaybeCPP hcp
    return $ TaskInfo n t s rs ei d ci hc

  destroy = c_destroyTaskInfo

  equalExceptDefaults (TaskInfo n id_ sid rs ei d ci hc) (TaskInfo n' id' sid' rs' ei' d' ci' hc') =
    n == n' && id_ == id' && sid == sid' && ci == ci' && hc == hc' && d == d' && and (zipWith equalExceptDefaults rs rs') && case (ei, ei') of
      (TaskCommand c, TaskCommand c') -> equalExceptDefaults c c'
      (TaskExecutor e, TaskExecutor e') -> equalExceptDefaults e e'
      _ -> False
